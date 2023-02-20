{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Data.Char

-- | Abstract Syntax Tree.
data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonString String
    | JsonNumber Integer
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

-- | Represents a parser that takes a string and may or may not return a parsed
-- value of type a.
newtype Parser a = Parser { run :: String -> Maybe (String, a) }

-- | Defines how to map a function over a parsed value, while leaving the
-- remaining input unchanged.
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (remainder, parsed) <- p input
        Just (remainder, f parsed)

-- | Defines how to apply functions and values within the Parser context.
instance Applicative Parser where
    pure value = Parser $ \input -> Just (input, value)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

-- | Defines how to create parsers that can be tried in sequence, one after the other.
instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

-- | Returns a parser that parses a single character equal to the given input
-- character, returning the character and the remaining input.
charP :: Char -> Parser Char
charP expected = Parser $ \case
    (char:chars) | char == expected -> Just (chars, expected) 
    _ -> Nothing

-- | Returns a parser that parses a single character equal to the given input
-- character, returning the character and the remaining input.
stringP :: String -> Parser String
stringP = traverse charP 

-- | Returns a parser that applies the given predicate to the input string,
-- returning the longest prefix of characters that satisfy the predicate and
-- the remaining input.
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
    let (token, remainder) = span f input
        in Just (remainder, token)

-- | Returns a parser that applies the given parser to the input string, and fails
-- if the resulting list is empty.
notNull :: Parser [a] -> Parser [a]
notNull (Parser parser) = Parser $ \input -> do
    (input', xs) <- parser input
    if null xs
        then Nothing
        else Just (input', xs)

-- | Parser that matches a string that is between double quotes (not considering
-- any double quotes that are escaped).
stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

-- | Parser that matches zero or more withespaces characters.
ws :: Parser String
ws = spanP isSpace

-- | Returns a parser that parses a sequence of elements separated by separators,
-- returning a list of elements.
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

-- | Parser that matches the string "null" in the input, returning a JsonNull value.
--
-- Examples:
--
-- >>> run jsonNull "something else"
-- Nothing
--
-- >>> run jsonNull "null"
-- Just ("", JsonNull)
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null" 

-- | Parser that matches the strings "true" or "false" in the input, returning
-- a JsonBool value.
--
-- Examples:
--
-- run jsonBool "true"
-- Just ("",JsonBool True)
--
-- >> run jsonBool "false"
-- Just ("",JsonBool False)
--
-- >> run jsonBool "else"
-- Nothing
jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
    where
        f "true" = JsonBool True
        f "false" = JsonBool False

-- | Parser that matches a non-empty sequence of digits in the input, returning
-- a JsonNumber value.
--
-- Examples:
--
-- >>> run jsonNumber "123abc"
-- Just ("abc",JsonNumber 123) 
--
-- >>> run jsonNumber "abc"
-- Nothing
jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
    where f digits = JsonNumber $ read digits


-- | Parser that matches a JSON string, returning a JsonString value.
--
-- Examples:
--
-- >>> run jsonString "\"abc\""
-- Just ("",JsonString "abc")
--
-- >>> run jsonString ""
-- Nothing
jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral 

-- | Parser that matches a JSON array, returning a JsonArray value.
--
-- Examples:
--
-- >>> run jsonArray "[1, 2, 3]"
-- Just ("",JsonArray [JsonNumber 1,JsonNumber 2,JsonNumber 3])
--
-- >>> run jsonArray ""
-- Nothing
jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

-- | Parser that matches a JSON object, return a JsonObject value.
--
-- Examples:
--
-- >>> run jsonObject "{ \"foo\": \"bar\", \"num\": 1 }"
-- Just ("",JsonObject [("foo",JsonString "bar"),("num",JsonNumber 1)])
--
-- >>> run jsonObject "{}"
-- Just ("",JsonObject [])
jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$>
  (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
  where
    pair =
        (\key _ value -> (key, value)) <$> stringLiteral <*>
        (ws *> charP ':' <* ws) <*>
        jsonValue

-- | Parser that match a JSON value, by trying to match each possible JSON value
-- in sequence, returning the first one that matches.
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO ()
main = undefined
