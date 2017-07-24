{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParserSpec where

import           Test.Hspec

import           Data.Foldable         (for_)
import           Language.Tiger.Parser
import           Language.Tiger.TH
import           System.FilePath.Glob  (glob)

spec :: Spec
spec = do
    describe "parsing examples" $ do
        exs <- runIO $ glob "test/examples/*.tig"
        for_ exs $ \ex ->
            it ex $ do
                decs <- parseFromFile ex
                decs `shouldSatisfy` (not . null)
    describe "small examples" $ do
        it "can parse empty string" $ do
            parseFromByteString ""
                `shouldBe`
                    Right []
        it "can parse a number" $ do
            parseFromByteString "3"
                `shouldBe`
                    Right [EInt 3]

        it "can parse a simple binop" $ do
            parseFromByteString "3 * 2"
                `shouldBe`
                    Right [BinOp (EInt 3) (ArithOp OTimes) (EInt 2)]
        it "can parse a let" $ do
            parseFromByteString "let var foo : int := 3 in 1 end"
                `shouldSatisfy` (not . null)

