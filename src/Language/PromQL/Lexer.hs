module Language.PromQL.Lexer
    (Token(..)) where

import Protolude

-- Token is the top-level type, followed by a depth-first traversal of the
-- various kinds of tokens.

data Token
  = Identifier Text
  | Metric Text
  | String Text
  | Comment Text
  | Num Number
  | Dur Duration
  | Op Operator
  | Key Keyword
  | Term Terminal
  | Agg Aggregator
  deriving (Eq, Show)

-- TODO: Figure out what to do for this. Not sure if we should stay generic
-- (Fractional?) or whether we should just leave it as Text.
type Number = Double
data Duration = Duration Number DurationUnit deriving (Eq, Show)

data DurationUnit
  = Second
  | Minute
  | Hour
  | Day
  | Week
  | Year
  deriving (Eq, Show)

data Operator
  = SUB
  | ADD
  | MUL
  | MOD
  | DIV
  | LAND
  | LOR
  | LUnless
  | EQL
  | NEQ
  | LTE
  | LSS
  | GTE
  | GTR
  | EQLRegex
  | NEQRegex
  | POW
  deriving (Eq, Show)

precedence :: Operator -> Int
precedence LOR     = 1
precedence LAND    = 2
precedence LUnless = 2
precedence EQL     = 3
precedence NEQ     = 3
precedence LTE     = 3
precedence LSS     = 3
precedence GTE     = 3
precedence GTR     = 3
precedence ADD     = 4
precedence SUB     = 4
precedence MUL     = 5
precedence DIV     = 5
precedence MOD     = 5
precedence POW     = 6
precedence _       = 0

data Associativity = LeftAssociative | RightAssociative deriving (Eq, Show)

associativity :: Operator -> Associativity
associativity POW = RightAssociative
associativity _   = LeftAssociative


-- XXX: I don't know what these should be called or even whether they deserve
-- to be a separate type.
data Terminal
  = EOF
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Comma
  | Assign
  | Semicolon
  | Blank
  | Times
  deriving (Eq, Show)

data Aggregator
  = Avg
  | Count
  | Sum
  | Min
  | Max
  | Stddev
  | Stdvar
  | TopK Param
  | BottomK Param
  | CountValues Param
  | Quantile Param
  deriving (Eq, Show)

type Param = Text -- XXX: Pick proper thing for this

data Keyword
  = Alert
  | If
  | For
  | Labels
  | Annotations
  | KeepCommon
  | Offset
  | By
  | Ignoring
  | GroupLeft
  | GroupRight
  | Bool
  deriving (Eq, Show)

