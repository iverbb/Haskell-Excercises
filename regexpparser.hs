module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

import Text.Parsec

data RegExp
  = Normal Char
  | Any
  | ZeroOrMore RegExp
  | Or RegExp RegExp
  | Str [RegExp]
  deriving (Show, Eq)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe output = case output of
  Left _ -> Nothing
  Right x -> Just x

parens :: Parsec String () a -> Parsec String () a
parens = between (char '(') (char ')')

parseRegExp :: String -> Maybe RegExp
parseRegExp = eitherToMaybe . parse parseExp ""

parseExp :: Parsec String () RegExp
parseExp = do
  expr <- try parseOr
          <|> try parseStr
          <|> parseUnit
  _    <- eof
  return expr

parseOr :: Parsec String () RegExp
parseOr = do
  s1 <- try parseStr <|> parseUnit
  _  <- char '|'
  s2 <- try parseStr <|> parseUnit
  return $ Or s1 s2

parseStr :: Parsec String () RegExp
parseStr = do
  h <- parseUnit
  t <- many1 parseUnit
  return $ Str $ h : t

parseUnit :: Parsec String () RegExp
parseUnit =
  try parseMany
  <|> try (parens parseOr)
  <|> try (parens parseStr)
  <|> parseNormal
  <|> parseAny
  <|> parens parseUnit

parseMany :: Parsec String () RegExp
parseMany = do
  expr <- parens (   try parseOr
                 <|> try parseStr
                 <|> try parseUnit)
          <|> parseNormal
          <|> parseAny
  _    <- char '*'
  return $ ZeroOrMore expr

parseNormal' :: Parsec String () Char
parseNormal' = try (parens parseNormal') <|> noneOf "()|*."

parseNormal :: Parsec String () RegExp
parseNormal = Normal <$> parseNormal'

parseAny :: Parsec String () RegExp
parseAny
  =   (char '.' >> return Any)
  <|> parens parseAny
