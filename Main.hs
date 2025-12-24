module Main where

import Control.Applicative
import Data.Char

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: This could be improved to support double
  | JsonString String -- NOTE: This could be improved to support escapes
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

data JsonOutput a
  = Success (a, String)
  | Failure String
  deriving (Show, Eq)

-- NOTE: We will use this complicated thing called a record.
-- NOTE: Haskell generates functions for these fields
newtype Parser a = Parser
  { runParser :: String -> JsonOutput a
  }

-- Make parser mappable
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) =
    Parser
      ( \input ->
          case p input of
            Success (x, input') -> Success (f x, input')
            Failure error -> Failure error
      )

-- <$> is a synonym to fmap
-- (,,,) <$> Just 1 <*> Just 2 <*> Just 3 <*> Just 4 => Just (1,2,3,4)
-- Make parser applicative
instance Applicative Parser where
  pure :: a -> Parser a
  pure x =
    Parser
      ( \input ->
          Success (x, input)
      )

  -- sequenceA (map parseChar "null") :: Parser [Char]
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) =
    Parser
      ( \input ->
          case p1 input of
            Success (func, input') ->
              case p2 input' of
                Success (arg, input'') ->
                  Success (func arg, input'')
                Failure error ->
                  Failure error
            Failure error ->
              Failure error
      )

-- Make parser alternative
-- NOTE: Picks first non empty
instance Alternative Parser where
  empty = Parser (\_ -> Failure "Alternative Property Error")
  (Parser p1) <|> (Parser p2) =
    Parser
      ( \input ->
          case p1 input of
            Success x -> Success x
            Failure _ -> p2 input
      )

-- Create parsers for each individual type first
-- Create a helper parser to parse a single char
parseChar :: Char -> Parser Char
parseChar x =
  Parser
    ( \input ->
        case input of
          head : tail | head == x -> Success (head, tail)
          _ -> Failure "parseChar"
    )

-- Create a helper parser to parse a string
parseString :: String -> Parser String
parseString = traverse parseChar

-- Create a parser that is capable to parse JsonNull
jsonNull :: Parser JsonValue
jsonNull = fmap (\_ -> JsonNull) (parseString "null")

-- Create a parser that is capable to parse JsonBool
jsonBool :: Parser JsonValue
jsonBool = fmap (\_ -> JsonBool True) (parseString "true") <|> fmap (\_ -> JsonBool False) (parseString "false")

-- NOTE: We need to use span with a predicate
parseSpan :: (Char -> Bool) -> Parser String
parseSpan f =
  Parser
    ( \input -> let (token, rest) = span f input in Success (token, rest)
    )

-- Make a helper decorator
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser
    ( \input -> case p input of
        Success (parsed, parsable) ->
          if null parsed
            then Failure "notNull"
            else Success (parsed, parsable)
        Failure error -> Failure error
    )

-- Create a parser that is capable to parse JsonNumber
jsonNumber :: Parser JsonValue
jsonNumber = fmap (\digit -> JsonNumber (read digit)) (notNull (parseSpan isDigit))

stringLiteral :: Parser String
stringLiteral = (parseChar '"' *> parseSpan (/= '"') <* parseChar '"')

-- Create a parser that is capable to parse JsonString
jsonString :: Parser JsonValue
jsonString = fmap JsonString stringLiteral

-- Create a parser that is capable to parse JsonArray
ws :: Parser String
ws = parseSpan isSpace

seperateBy :: Parser a -> Parser b -> Parser [b]
seperateBy delim element = fmap (:) element <*> many (delim *> element) <|> pure []

-- Example: runParser (many jsonNull) "nullnullnull" => Success ([JsonNull,JsonNull,JsonNull],"")
jsonArray :: Parser JsonValue
jsonArray = fmap JsonArray (parseChar '[' *> ws *> elements <* ws <* parseChar ']')
  where
    elements = seperateBy (ws *> parseChar ',' <* ws) jsonValue

-- Create a parser that is capable to parse JsonObject
jsonObject :: Parser JsonValue
jsonObject =
  fmap
    JsonObject
    ( parseChar '{'
        *> ws
        *> seperateBy (ws *> parseChar ',' <* ws) pair
        <* ws
        <* parseChar '}'
    )
  where
    pair =
      (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> parseChar ':' <* ws) <*> jsonValue

-- Create a parser that is capable to parse JsonValue
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (JsonOutput a)
parseFile fileName parser = do
  input <- readFile fileName
  return (runParser parser input)

main :: IO ()
main = undefined
