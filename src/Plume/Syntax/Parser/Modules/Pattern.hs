module Plume.Syntax.Parser.Modules.Pattern where

import Control.Monad.Parser
import Plume.Syntax.Common.Pattern
import Plume.Syntax.Parser.Lexer
import Plume.Syntax.Parser.Modules.Literal hiding (parseLiteral)
import Text.Megaparsec hiding (some)

parsePattern :: Parser Pattern
parsePattern =
  choice
    [ parseWildcard
    , parseTuple
    , parseList
    , parseLiteral
    , parseConstructor
    , parseVariable
    ]

parseVariable :: Parser Pattern
parseVariable = PVariable <$> identifier

parseTuple :: Parser Pattern
parseTuple = do
  items <- parens (parsePattern `sepBy` comma)
  return $ buildTuple items
 where
  buildTuple [] = PVariable "unit"
  buildTuple [x] = x
  buildTuple (x : xs) = PConstructor "tuple" [x, buildTuple xs]

parseLiteral :: Parser Pattern
parseLiteral =
  PLiteral
    <$> choice
      [ parseString
      , parseChar
      , parseBool
      , try parseFloat
      , parseInteger
      ]

isSlice :: Pattern -> Bool
isSlice (PSlice _) = True
isSlice _ = False

parseList :: Parser Pattern
parseList =
  brackets $ do
    items <- (parseSlice <|> parsePattern) `sepBy` comma
    (items', slice) <- case reverse items of
      [] -> return ([], Nothing)
      (p@(PSlice _) : rest)
        | not (any isSlice rest) -> return (reverse rest, Just p)
      _ | not (any isSlice items) -> return (items, Nothing)
        | otherwise -> fail "invalid slice position"
    return $ PList items' slice

parseSlice :: Parser Pattern
parseSlice = PSlice <$> (symbol ".." *> identifier)

parseConstructor :: Parser Pattern
parseConstructor = do
  name <- try $ identifier <* symbol "("
  args <- parsePattern `sepBy` comma
  _ <- symbol ")"
  return $ PConstructor name args

parseWildcard :: Parser Pattern
parseWildcard = PWildcard <$ symbol "?"
