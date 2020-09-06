module BfHs.Language 
    ( BfAstNode (..),
      BfProgram,
      parseBrainFuck )
    where
    
import Text.Parsec.Combinator 
    ( between, 
      sepEndBy )

import Text.Parsec.Char 
    ( char, 
      spaces )

import Text.ParserCombinators.Parsec
    ( char,
      spaces,
      between,
      sepEndBy,
      (<|>),
      parse,
      ParseError,
      Parser )

{-| 
    The data constructor representing an abstract syntax tree node in a brainfuck program.
-}
data BfAstNode 
    = MoveLeft          -- ^ Moves the data pointer to the left (<).
    | MoveRight         -- ^ Moves the data pointer to the right (>).
    | Increment         -- ^ Increments the value referenced by the data pointer by one (+)
    | Decrement         -- ^ Decrements the value referenced by the data pointer by one (-)
    | Output            -- ^ Writes the value referenced by the data pointer to the standard output (+).
    | Input             -- ^ Reads a single value from the standard input to the cell referenced by the data pointer.
    | Loop [BfAstNode]  -- ^ Describes a loop that repeats execution until the value referenced by the data pointer is 0 ([]).
    deriving (Show)

{-|
    Defines a brainfuck program
-}
type BfProgram = [BfAstNode]

{-
    The following defines the grammar of the brainfuck language.
-}

program :: Parser BfProgram
program = sepEndBy statement spaces

statement :: Parser BfAstNode
statement 
  = simpleStatement
 <|> loopStatement

simpleStatement :: Parser BfAstNode
simpleStatement = fmap construct simpleStatementChar
    where 
        construct '>' = MoveRight
        construct '<' = MoveLeft 
        construct '+' = Increment
        construct '-' = Decrement
        construct '.' = Output
        construct ',' = Input
        simpleStatementChar
            =  char '>'
            <|> char '<' 
            <|> char '+' 
            <|> char '-'
            <|> char '.' 
            <|> char ','

loopStatement :: Parser BfAstNode
loopStatement = Loop <$> between (char '[') (char ']') program

{-|
    Parses an input string as a brainfuck program.
-}
parseBrainFuck :: String -> Either ParseError BfProgram
parseBrainFuck s = parse program "" (filter isBrainFuckChar s)
    where 
        isBrainFuckChar c = c `elem` "><+-.,[]"