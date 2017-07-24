{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module LexerSpec where

import           Control.Exception    (evaluate)
import           Data.Foldable        (for_)
import           System.FilePath.Glob (glob)
import           Test.Hspec

import           Language.Tiger.Lexer
import           Language.Tiger.TH

spec :: Spec
spec = do
    describe "let" $
        it "can lex" $
            map tokenTy [tigerl|let foo : int = 3 in foo end|]
                `shouldBe`
                    [Let, Ident "foo", Colon, Ident "int", EqualSym, LitInt 3,
                        In, Ident "foo", End]

    describe "LitStr" $ do
        it "can lex" $
            map tokenTy [tigerl|foo = "hello"|]
                `shouldBe`
                    [Ident "foo", EqualSym, LitStr "hello"]
        it "handles control characters" $
            map tokenTy [tigerl|foo = "\n"|]
                `shouldBe`
                    [Ident "foo", EqualSym, LitStr "\\n"]

    describe "lexing examples" $ do
        exs <- runIO $ glob "test/examples/*.tig"
        for_ exs $ \ex -> do
            decs <- runIO (lexFromFile ex)
            it ex $ do
                for_ decs evaluate
                decs `shouldSatisfy` (not . null)

