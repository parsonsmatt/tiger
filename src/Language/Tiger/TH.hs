{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Tiger.TH where

import qualified Data.ByteString.Lazy.Char8 as C8
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Exercises.Ch2.TigerScanner as Lexer
import           Exercises.Ch3.TigerParser

tigerl :: QuasiQuoter
tigerl = QuasiQuoter
    { quoteDec = err
    , quoteType = err
    , quotePat = err
    , quoteExp = lexTigerToExpr
    }
  where
    err = error "tigerl only for use in expressions"

tigerp :: QuasiQuoter
tigerp = QuasiQuoter
    { quoteDec = err
    , quoteType = err
    , quotePat = err
    , quoteExp = parseTigerToExpr
    }
  where
    err = error "tigerp only for use in expressions"

parseTigerToExpr :: String -> Q Exp
parseTigerToExpr str = do
    let lexed = Lexer.alexScanTokens (C8.pack str)
        parsed = parse lexed
    [| parsed|]

lexTigerToExpr :: String -> Q Exp
lexTigerToExpr str = [|Lexer.alexScanTokens (C8.pack str)|]
