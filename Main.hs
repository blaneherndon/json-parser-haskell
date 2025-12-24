module Main where

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
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
-- NOTE: Picks first non empty
jsonBool :: Parser JsonValue
jsonBool =
  Parser
    ( \input ->
        case runParser (parseString "true") input of
          Success (parsed, parsable) -> Success (JsonBool True, parsable)
          Failure _ ->
            case runParser (parseString "false") input of
              Success (parsed, parsable) -> Success (JsonBool False, parsable)
              Failure error -> Failure error
    )

-- Create a parser that is capable to parse JsonValue
jsonValue :: parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined
