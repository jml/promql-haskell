module Language.PromQL.Model
  ( LabelSet
  , LabelName
  , LabelNames
  , LabelValue
  -- * Core types
  , SampleValue
  , Duration
  , Time
  , Regex
  , ValueType
  ) where

import Protolude

type LabelSet = Map LabelName LabelValue

newtype LabelName = LabelName Text deriving (Eq, Show)

type LabelNames = [LabelName]

newtype LabelValue = LabelValue Text deriving (Eq, Show)


-- | A value for a given sample at a given time.
-- XXX: Should be a 64 bit float. Not sure how to specify this in Haskell.
--
-- XXX: Can we make this generic? Prometheus needs this to be float64 for a
-- variety of reasons, but maybe in Haskell we can get away with being generic.
newtype SampleValue = SampleValue Float deriving (Eq, Ord, Show)

-- | Number of milliseconds since the epoch (1970-01-01 00:00 UTC) excluding
-- leap seconds.
newtype Time = Time Integer

-- | Elapsed time between two instants as a nanosecond count.
--
-- XXX: Perhaps we should be using some Haskell standard type here. Defining
-- our own for now for simplicity while figuring out exactly how the PromQL
-- language works.
newtype Duration = Duration Integer

data ValueType = Scalar | Vector | Matrix | String


-- | A regular expression.
data Regex
