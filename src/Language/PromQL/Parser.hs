module Language.PromQL.Parser () where

import Protolude

import qualified Language.PromQL.Model as Model


data Statement
  = Alert Name Expression Model.Duration Model.LabelSet Model.LabelSet
  | Eval Expression Model.Time Model.Time Model.Duration
  | Record Name Expression Model.LabelSet

data Expression
  = Aggregate AggregateExpression
  | Binary BinaryExpression
  | Call CallExpression
  | MatrixSelector MatrixSelectorExpression
  | Number Model.SampleValue
  | ParenExpression Expression
  | String Text
  | Unary UnaryExpression
  | VectorSelector VectorSelectorExpression

data AggregateExpression =
  AggregateExpression { aggregateOp :: Operator
                      , expression :: Expression
                      , param :: Expression
                      , grouping :: Model.LabelNames
                      , without :: Bool
                      , keepCommonLabels :: Bool
                      }

data BinaryExpression =
  BinaryExpression { binaryOp :: Operator
                   , lhs :: Expression
                   , rhs :: Expression
                   , vectorMatching :: Maybe VectorMatching
                   , returnBool :: Bool
                   }

data CallExpression =
  CallExpression { function :: Function
                 , args :: [Expression]
                 }

data MatrixSelectorExpression =
  MatrixSelectorExpression { name :: Name
                           , range :: Model.Duration
                           , offset :: Model.Duration
                           , labelMatchers :: LabelMatchers
                           }

data UnaryExpression =
  UnaryExpression Operator Expression

data VectorSelectorExpression =
  VectorSelectorExpression { vsName :: Name
                           , vsOffset :: Model.Duration
                           , vsLabelMatchers :: LabelMatchers
                           }

type Name = Text

data Function
  = Function Name [Model.ValueType] Int Model.ValueType
  -- XXX: Int is optional args. Probably should just have an optional args list.
  --
  -- XXX: Golang definition has a Call method. I don't know how to do that in
  -- Haskell.

type LabelMatchers = [LabelMatcher]

data LabelMatcher
  = Equal Model.LabelName Model.LabelValue
  | NotEqual Model.LabelName Model.LabelValue
  | RegexMatch Model.LabelName Model.Regex
  | RegexNoMatch Model.LabelName Model.Regex

data Operator

data VectorMatching
  = VectorMatching
    { cardinality :: VectorMatchCardinality
    , matchingLabels :: Model.LabelNames
    , on :: Bool
    , include :: Model.LabelNames
    }

data VectorMatchCardinality
  = OneToOne
  | ManyToOne
  | OneToMany
  | ManyToMany
  deriving (Eq, Show)
