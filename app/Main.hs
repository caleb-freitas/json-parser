{-# LANGUAGE LambdaCase #-}

module Main where

-- | Abstract Syntax Tree
data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonString String
    | JsonNumber Integer
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

newtype Parser a = Parser { run :: String -> Maybe (String, a) }

-- | Maps the given function over the parsed value of a parser, leaving the
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

-- | Parses a single character equal to the given input character, returning the
-- character and the remaining input.
charP :: Char -> Parser Char
charP expected = Parser $ \case
    (char:chars) | char == expected -> Just (chars, expected) 
    _ -> Nothing

-- | Parses a string by matching each character of the input string with a given
-- target string, returning the matched string and the remaining input.
stringP :: String -> Parser String
-- | The `traverse` function apply `charP` to each character in the target string.
stringP = traverse charP 

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null" 

main :: IO ()
main = putStrLn "Hello, Haskell!"
