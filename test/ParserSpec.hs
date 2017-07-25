{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ParserSpec where

import           Test.Hspec

import           Data.Either           (isRight)
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
                putStrLn $ "doing the thing with" ++ ex
                decs `shouldSatisfy` isRight
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

        it "can parse an if" $ do
            parseFromByteString "if true then false else true"
                `shouldBe`
                    Right [EIfElse (ELValue (LIdent (Ident "true"))) (ELValue (LIdent (Ident "false"))) (ELValue (LIdent (Ident "true")))]

            parseFromByteString "if a=nil then b else a"
                `shouldBe`
                    let lval = ELValue . LIdent . Ident in
                    Right [EIfElse (BinOp (lval "a") (CompOp OEQ) ENil) (lval "b") (lval "a")]

            parseFromByteString "let function merge(a: list, b: list) : list = if a=nil then b else a in foo end"
                `shouldSatisfy`
                    isRight

        it "can parse a function" $ do
            parseFromByteString "let function skipto() = nil in end"
                `shouldSatisfy`
                    isRight

        it "can parse an lvalue" $ do
            parseFromByteString "let var d:=0 in d[3] end"
                `shouldBe`
                    Right
                        [ELet
                            [VarDec $ mkVarDecl "d" Nothing (EInt 0)]
                            (ELValue (LSubscript (LIdent "d") (EInt 3)))]
