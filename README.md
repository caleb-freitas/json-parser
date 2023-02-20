# JSON Parser

This module implements a simple JSON parser that can parse JSON objects and arrays. The parser converts valid JSON strings into an Abstract Syntax Tree (AST) of JsonValue types.

## Requirements

The code requires a Haskell compiler to run (tested only with GHC).

## Usage

The `jsonValue` function is the main entry point for the JSON parser:

```haskell
jsonValue :: Parser JsonValue
```

This function parses a JSON value from a string and returns an instance of the JsonValue type.

The supported JSON data types are:

- `JsonNull`

- `JsonBool`

- `JsonString`

- `JsonNumber`

- `JsonArray`

- `JsonObject`

Each data type has its own parser, which can be used separately if desired. The functions are:

- `jsonNull`

- `jsonBool`

- `jsonString`

- `jsonNumber`

- `jsonArray`

- `jsonObject`

All of the above parsers are exported and can be used to parse specific JSON data types.

If you want to check how to use each of the above parsers, please check the docummentation comments in the source code at the [main file](./app/Main.hs).

## Main Topics Covered

- Applicatives.

- Functors.

- Parsing.

## Reference

All this code was developed while watching [this tutorial](https://www.youtube.com/watch?v=N9RUqGYuGfw) from [Tsoding](https://tsoding.github.io/).
