{-# LANGUAGE QuasiQuotes #-}

import Protolude

import NeatInterpolation (text)
import Data.Text (strip)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
import Text.Parsec (parse)

import Language.PromQL.Model (makeDuration, DurationUnits(..))
import Language.PromQL.Parser (duration)


main :: IO ()
main = do
  allTests <- tests
  defaultMain allTests

tests :: IO TestTree
tests = do
  parser <- parserSpec
  pure $ testGroup "Language.PromQL"
    [ parser
    ]

parserSpec :: IO TestTree
parserSpec =
  testSpec "Language.PromQL.Parser" $ do
    describe "duration" $ do
      it "parses numbers" $ do
        parse duration "" "42h" `shouldBe` Right (makeDuration 42 Hours)
      it "gives nice errors on non-units" $ do
        let Left e = parse duration "" "42u"
        show e `shouldBe` (strip [text|
                                      (line 1, column 3):
                                      unexpected "u"
                                      expecting digit or valid time unit in duration string|])
