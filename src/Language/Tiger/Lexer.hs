module Language.Tiger.Lexer
    ( Token(..)
    , TokenTy(..)
    , AlexPosn
    , lexTiger
    , lexFromFile
    ) where

import           Data.ByteString.Lazy       (ByteString)
import           Exercises.Ch2.TigerScanner

lexTiger :: ByteString -> [Token]
lexTiger = alexScanTokens
