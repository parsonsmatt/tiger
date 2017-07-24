module Language.Tiger.Parser
    ( module Exercises.Ch3.TigerParser
    , module Language.Tiger.Parser
    ) where

import           Data.ByteString.Lazy.Char8 as C8
import           Exercises.Ch3.TigerParser
import           Language.Tiger.Lexer

parseFromByteString :: ByteString -> Either String [Expr]
parseFromByteString = parse . lexTiger

parseFromFile :: FilePath -> IO (Either String [Expr])
parseFromFile = fmap parseFromByteString . C8.readFile
