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

