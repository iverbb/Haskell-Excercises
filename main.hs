{-# LANGUAGE BangPatterns #-}
module Main
  ( main
  , readC
  ) where


import Data.List.Split
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Console.ANSI
import System.Console.ANSI.Types
import System.Directory
import System.Exit
import System.IO
import System.IO.Error
import System.Posix.Signals
import System.Process

-- Datatypes for handling commands.
-- History: event index and list of executed commands
-- Linestate: cursorindex and line
-- Tribool, just a way for me to force matching on yes, no, maybe
-- would make sure all cases are covered if a function expected this type
data History = History Int [String]

data LineState = LineState Int String

data TriBool = Valid | Unknown | Invalid

-- Get all files on user defined path, doesn't actually verify 
-- if they are executable
getExecs :: [String] -> IO [String]
getExecs path =
  readProcess "/bin/ls" path []
  >>= return . words

-- Find all executables that match the current typed line
filterLine :: String -> [String] -> [String]
filterLine line execs =
  case line of
    "" -> execs
    _  ->
      let cmd = head $ words line
          len = length cmd
      in filter (\exe -> take len exe == cmd) execs

-- Check if there are any valid commands corresponding to the current line
-- Agnostic if line starts with . or /
validCmd :: String -> [String] -> TriBool
validCmd line path 
  | line == "" || elem (head line) "./" 
  = Unknown
  | otherwise =
    case suggs of 
      [] -> Invalid
      _  -> Valid
    where suggs = filterLine line path

-- Color code based on validity of command
colorSignal :: TriBool -> IO ()
colorSignal val =
  case val of
    Valid   -> setSGR [SetColor Foreground Dull Cyan]
    Unknown -> setSGR [SetColor Foreground Dull White]
    Invalid -> setSGR [SetColor Foreground Dull Red]
--

-- Mkdir with ability to create necessary parent directories
-- Both this and the CD has some issues with ambiguous error messages
-- Didn't have time to look into how to improve them properly
mkdir :: String -> IO ()
mkdir name = 
  catch (createDirectoryIfMissing True name)
        (\e ->putStrLn "invalid input"
              >> print (e::IOException))

-- CD, change directory
-- TODO: extend with tab completion
-- TODO: proper error message
cd :: String -> IO ()
cd path = 
  catch (setCurrentDirectory path)
        (\e -> putStrLn "invalid input"
               >> print (e::IOException))

-- Print a prompt displaying current directory
-- TODO: include functionality for position so it doesn't need be hard coded
-- in every case of the main loop
printPrompt :: String -> IO ()
printPrompt line =
  clearLine 
  >>  cursorUp 1
  >>  getCurrentDirectory
  >>= putStrLn
  >>  putStr (">> " <> line)

-- Experimental tab completion function, didn't get to integrate it yet
tabComplete :: String -> [String] -> IO String
tabComplete line execs =
  case (length options) of
    0 -> return line
    1 -> return $ head options
    _ -> displayOptions line options
         >> return line
  where options = filterLine line execs

-- Experimental display function, no time to properly integrate
displayOptions :: String -> [String] -> IO ()
displayOptions line options =
  cursorDown 1
  >> clearLine
  >> putStr (unwords options)
  >> cursorUp 1
  >> clearLine
  >> setCursorColumn 0
  >> putStr (">> " <> line)

-- Parse line and split at "||" to produce list of commands
readCmds :: String -> [[String]]
readCmds = (words <$>) . splitOn "||"

-- Create a new thread to execute the command
threadedProc :: [String] -> IO ()
threadedProc (exe:args) = 
  forkIO (simpleProc (exe:args))
  >>= print

-- Run a process with the given arguments, does not use any shell, but rather
-- goes for binaries on the path
simpleProc :: [String] -> IO ()
simpleProc (exe:args) = 
  catch (readProcessWithExitCode exe args []
        >>= \(code, out, err) -> putStr out)
        (\e  -> putStrLn $ show (e :: IOException))

-- Determine if command is a binary, a function written in this file,
-- if to be run in separate thread or not, then execute
execute :: [String] -> IO ()
execute []  = pure ()
execute (cmd:args) =
  case cmd of
    "cd" -> cd (head args')
    "mkdir" -> mkdir (head args')
    _       -> case (last args') of
      "&" -> threadedProc (cmd:(init args'))
      _   -> simpleProc (cmd:(init args'))
  where args' =
          -- Hacky fix to workaround "empty list" issues
          case args of
            [] -> [""]
            _  -> args<>[""]


-- Parse multiple characters input at once, arrow keys are registered
-- as three character codes for some reason
getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where 
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char : chars)

-- Main loop, reads character from stdin, takes proper action.
-- Currently supported functionality:
-- Color coding according to whether command is recognized or not
-- cursor movement with left and right arrow
-- character deletion with backspace
-- displaying 5 first executables on path on tab
-- exit with "exit"
-- TODO: extend to read validate multiple command in case of multicommand lines
-- TODO: add character deletion with DEL
-- TODO: handle other special characters properly, currently messes up display somewhat
-- TODO: extend history scroller to save current line, and also to include an
-- empty line in the scrolling sequence
-- currently only reads first command

-- Take a linestate(line plus cursor position), a list of known executables
-- (used for color coding), a history of executed lines, and then read a next 
-- character to determine next appropriate action
readC :: LineState-> [String] -> History -> IO ()
readC lineState@(LineState pos line) execs history@(History gen record) =
  hSetBuffering stdin NoBuffering
  >>  getKey
  >>= \c ->
    when (c /= "\ESC") $ do
      case c of
        -- Arrow key up
        -- Increment the index of the history to display an earlier event
        "\ESC[A" -> pure (validCmd event execs)
                    >>= \val -> return val
                    >>  colorSignal val
                    >>  setCursorColumn 0
                    >>  printPrompt event
                    >>  setCursorColumn (pos' + 3)
                    >>  readC (LineState (length event) event) execs (History gen' record)
                      where gen'    = mod (gen+1) (length record)
                            event   = record!!gen'
                            pos'    = length event
        -- Arrow key down
        -- Decrement history index
        "\ESC[B" -> pure (validCmd event execs)
                    >>= \val -> return val
                    >>  colorSignal val
                    >>  setCursorColumn 0
                    >>  printPrompt event
                    >>  setCursorColumn (pos' + 3)
                    >>  readC (LineState (length event) event) execs (History gen' record)
                      where gen'    = mod (gen-1) (length record)
                            event   = record!!gen'
                            pos'    = length event
        -- Right arrow key
        -- Increment position by one if not last column
        "\ESC[C" -> setCursorColumn (3 + pos')
                    >> readC (LineState pos' line) execs history
                      where pos' = min (length line) (pos + 1)
        -- Left arrow key
        -- Decrement position by one if not first column
        "\ESC[D" -> setCursorColumn (pos' + 3)
                    >> readC (LineState pos' line) execs history
                      where pos' = max 0 (pos - 1)
        -- Tab: display first 5 items on path that match current line
        -- Note: path currently hardcoded to be "/bin" and "/sbin"
        -- This has no bearing on execution, but limits autocompletion and 
        -- color coding
        "\t" -> getExecs ["/bin", "/sbin"]
                >>= return . filterLine line
                >>= \suggestions -> return suggestions
                >>  cursorDown 1
                >>  clearLine
                >>  setCursorColumn 0
                >>  putStr (unwords $ take 5 suggestions)
                >>  cursorUp 1
                >>  clearLine
                >>  setCursorColumn 0
                >>  printPrompt line
                >>  readC lineState execs history
        -- Enter: in case of empty line -> Noop
        --                   "exit"     -> stop program
        --                   otherwise  -> try to execute current line
        "\n" -> case line of
                  -- Match empty string
                  ""     -> readC lineState execs history
                  -- Match string "exit"
                  -- Only matches on exact string currently
                  -- TODO: match on any string where "exit" is first word
                  "exit" -> clearLine
                            >> setCursorColumn 0
                            >> putStrLn "exit"
                            >> putStrLn "Exiting SW4gShell, buhbye!"
                  -- Wildcard match, catches anything not previously matched
                  _      -> clearLine
                            >> setCursorColumn 0
                            >> putStrLn line
                            >> clearLine
                            >> mapM_ execute (readCmds line)
                            >> putStrLn ""
                            >> printPrompt ""
                            >> readC (LineState 0 "") 
                                      execs (History 0 (line:record))
        -- Backspace key: remove one character left of pointer
        "\DEL"  -> case left of
                  -- In case of empty line: noop
                  "" -> readC lineState execs history
                  _  -> clearLine
                        >>  pure (validCmd (line') execs)
                        >>= \val -> return val
                        >>  colorSignal val
                        >>  setCursorColumn 0
                        >>  printPrompt line'
                        >>  setCursorColumn (pos' + 3)
                        >>  readC (LineState pos' line') execs history
                          where line' = init left <> right
                                pos'  = max 0 (pos - 1)
                    where (left, right) = splitAt pos line
        -- For any other characters: add to line and run the reader again
        _    -> pure (line <> c)
                >>= \line' -> return line'
                >>  pure (validCmd (line') execs)
                >>= \val -> return val
                >>  colorSignal val
                >>  clearLine
                >>  setCursorColumn 0
                >>  printPrompt line'
                >>  setCursorColumn (pos + 4)
                >>  readC (LineState (pos + 1) line') execs history

-- Prepare program by suppressing ctrl-c, input and output buffering
-- Setting up path for color coding and suggestions as "/bin" and "/sbin"
-- TODO: use system $PATH instead
main :: IO ()
main = do
  _ <- installHandler keyboardSignal Ignore Nothing
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  printPrompt ""
  execs <- getExecs ["/bin", "/sbin"]
  readC (LineState 0 "") execs (History 0 [])
