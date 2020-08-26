module LispLovesMe where

import Text.Parsec

data AST
  = I32 Int
  | Sym String
  | Nul
  | Err
  | Lst [AST]
  | Boo Bool
  | Nod AST [AST]
  deriving (Eq, Show)

astArithm :: (Int -> Int -> Int) -> AST -> AST -> AST
astArithm op (I32 a) (I32 b) = I32 $ op a b
astArithm _ _ _ = Err

astNot :: [AST] -> AST
astNot [Boo b] = Boo $ not b
astNot _ = Err

astRange :: [AST] -> AST
astRange [I32 a, I32 b] = Lst $ I32 <$> [a .. b]
astRange _ = Err

astEq :: [AST] -> AST
astEq [I32 a, I32 b] = Boo $ a == b
astEq _ = Err

astNotEq :: [AST] -> AST
astNotEq = astNot . pure . astEq

astIf :: [AST] -> AST
astIf [Boo a, b] = if a then b else Nul
astIf [Boo a, b, c] = if a then b else c
astIf _ = Err

astList :: [AST] -> AST
astList = Lst

astSize :: [AST] -> AST
astSize [Lst a] = I32 $ length a
astSize _ = Err

astReverse :: [AST] -> AST
astReverse [Lst a] = Lst $ foldl (flip (:)) [] a
astReverse _ = Err

astBinCmp :: (Int -> Int -> Bool) -> [AST] -> AST
astBinCmp op [I32 a, I32 b] = Boo $ op a b
astBinCmp _ _ = Err

astCompare :: (Int -> Int -> Bool) -> [AST] -> AST
astCompare op (a:xs) =
  astBinCmp op
  [a, foldl (\(I32 b) (I32 c) -> if b > c then I32 b else I32 c) (I32 (minBound::Int)) xs]
astCompare _ _ = Err

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", foldl (astArithm (+)) (I32 0))
  , ("*", foldl (astArithm (*)) (I32 1))
  , ("-", \a ->
        if null a
        then Err
        else
          if length a == 1
          then (\(I32 x) -> I32 $ -1 * x) $ head a
          else foldl1 (astArithm (-)) a)

  , ("/", foldl1 (astArithm div))
  , ("^", \a ->
        if null a
        then Err
        else
          if length a == 1
          then head a
          else foldl1 (astArithm (^)) a)

  , (">", astCompare (>)) -- head astcmp max tail
  , ("<", astCompare (<))
  , ("!", astNot)
  , ("list", astList)
  , ("size", astSize)
  , ("reverse", astReverse)
  , ("..", astRange)
  , ("==", astEq)
  , (">=", astCompare (>=))
  , ("<=", astCompare (<=))
  , ("!=", astNotEq)
  , ("if", astIf)
  ]

--

applyPrelude :: [(String, [AST] -> AST)] -> String -> ([AST] -> AST)
applyPrelude prel str = case filter (\(s,_) -> s == str) prel of
  (_, func):_ -> func
  []          -> const Err

parseFinal :: Parsec String () AST
parseFinal = spaces >> try (parseRaw <|> parseNode)

parseNode :: Parsec String () AST
parseNode = do
  _ <- char '('
  h <- parseExpression
  t <- many (spaces >> try parseExpression)
  _ <- try spaces
  _ <- char ')'
  case t of
    [] -> case h of
      Sym _ -> return Err
      _     -> return h
    _  -> case h of
            Sym a -> pure $ f a t
            _     -> pure Err
  where f = applyPrelude preludeFunctions

parseNulls :: Parsec String () AST
parseNulls = (string "()" <|> string "null") >> pure Nul

parseBooleans :: Parsec String () AST
parseBooleans = parseTrue <|> parseFalse

parseFalse :: Parsec String () AST
parseFalse = string "false" >> pure (Boo False)

parseTrue :: Parsec String () AST
parseTrue = string "true" >> pure (Boo True)

parseSymbol :: Parsec String () AST
parseSymbol = do
  _ <- spaces
  h <- noneOf (" ,()\n\r\t" ++ ['0'..'9'])
  t <- many (noneOf "() ,\n\t\r")
  return $ Sym $ h : t

parseNumbers :: Parsec String () AST
parseNumbers = I32 . read <$> many1 digit

parseRaw :: Parsec String () AST
parseRaw = do
  _ <- spaces
  r <-
    choice
    ( try <$>
    [ parseNumbers
    , parseBooleans
    , parseNulls
    , parseSymbol])
  _ <- spaces
  return r

parseExpression :: Parsec String () AST
parseExpression = do
  _    <- spaces
  expr <-
    choice
    ( try <$>
      [ parseNumbers
      , parseBooleans
      , parseNulls
      , parseNode
      , parseSymbol
      ])
  _ <- spaces
  return expr

lispPretty :: String -> Maybe String
lispPretty s = case lispEval s of
  Nothing -> Nothing
  Just  _ -> Just $ lispFormat $ unwords $ words s

lispFormat :: String -> String
lispFormat [] = []
lispFormat [x] = [x]
lispFormat ('(':' ':xs) = '(' : lispFormat xs
lispFormat (' ':')':xs) = ')' : lispFormat xs
lispFormat (x:y:xs) = x : lispFormat (y:xs)

lispEval :: String -> Maybe AST
lispEval s = case parse parseFinal "" s of
  Left  _ -> Nothing
  Right x -> Just x
