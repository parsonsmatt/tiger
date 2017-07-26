{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import           Test.Hspec

import           Data.Either           (isRight)
import           Data.Foldable         (for_)
import           Data.List             (isInfixOf)
import           Language.Tiger.Parser
import           System.FilePath.Glob  (glob)

{-# ANN module ("HLint: Ignore redundant do" :: String) #-}

spec :: Spec
spec = do
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
            parseFromByteString "let function skipto() = nil in () end"
                `shouldSatisfy`
                    isRight

        it "can parse function call" $ do
            parseFromByteString "foobar()"
                `shouldBe`
                    Right [ FunCall "foobar" [] ]
            parseFromByteString "foobar(1,2)"
                `shouldBe`
                    Right [ FunCall "foobar" [EInt 1, EInt 2] ]

        it "can parse an lvalue" $ do
            parseFromByteString "d[3]"
                `shouldBe`
                    Right
                        [ELValue (LSubscript (LIdent "d") (EInt 3))]

        it "can assign" $ do
            parseFromByteString "a := 1"
                `shouldBe`
                    Right [EAssign (LIdent "a") (EInt 1)]

        it "can dot access" $ do
            parseFromByteString "a.foo"
                `shouldBe`
                    Right [ELValue $ LDot (LIdent "a") "foo"]

        it "can assign to field access" $ do
            parseFromByteString "a.foo := 1"
                `shouldBe`
                    Right [EAssign (LDot (LIdent "a") "foo") (EInt 1)]

        it "can assign to an array" $ do
            parseFromByteString "d[0] := 1"
                `shouldBe`
                    Right
                        [ EAssign (LSubscript (LIdent "d") (EInt 0)) (EInt 1)
                        ]

    describe "parsing examples" $ do
        exs <- runIO $ glob "test/examples/*.tig"
        for_ exs $ \ex ->
            it ex $
                if "49" `isInfixOf` ex then pure ()
                else  do
                    decs <- parseFromFile ex
                    decs `shouldSatisfy` isRight
