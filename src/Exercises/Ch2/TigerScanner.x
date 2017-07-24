{
module Exercises.Ch2.TigerScanner where

import qualified Data.ByteString.Lazy.Char8 as C8
}

%wrapper "posn-bytestring"

$digit = 0-9                      -- digits
$alpha = [a-zA-Z]                -- alphabetic characters
$graphic    = $printable # $white
 
@string     = \" (. # \")* \"

tokens :-

  $white+                       ;
  "//".*                        ;
  \;                            { \p _ -> T p Semicolon       }
  let                           { \p _ -> T p Let             }
  in                            { \p _ -> T p In              }
  while                         { \p _ -> T p While           }
  for                           { \p _ -> T p For             }
  to                            { \p _ -> T p To              }
  break                         { \p _ -> T p Break           }
  end                           { \p _ -> T p End             }
  function                      { \p _ -> T p Function        }
  var                           { \p _ -> T p Var             }
  type                          { \p _ -> T p Type            }
  array                         { \p _ -> T p Array           }
  if                            { \p _ -> T p If              }
  then                          { \p _ -> T p Then            }
  else                          { \p _ -> T p Else            }
  do                            { \p _ -> T p Do              }
  of                            { \p _ -> T p Of              }
  nil                           { \p _ -> T p Nil             }
  \,                            { \p _ -> T p Comma           }
  \:                            { \p _ -> T p Colon           }
  \(                            { \p _ -> T p OpenParen       }
  \)                            { \p _ -> T p CloseParen      }
  \[                            { \p _ -> T p OpenBrace       }
  \]                            { \p _ -> T p CloseBrace      }
  \{                            { \p _ -> T p OpenBracket     }
  \}                            { \p _ -> T p CloseBracket    }
  \.                            { \p _ -> T p Period          }
  \+                            { \p _ -> T p PlusSym         }
  \*                            { \p _ -> T p MulSym          }
  \-                            { \p _ -> T p SubSym          }
  \/                            { \p _ -> T p ForwardSlash    }
  \=                            { \p _ -> T p EqualSym        }
  \<\>                          { \p _ -> T p NotEqSym        }
  \<                            { \p _ -> T p LessThan        }
  \>                            { \p _ -> T p GreaterThan     }
  \>\=                          { \p _ -> T p AtLeast         }
  \<\=                          { \p _ -> T p AtMost          }
  \&                            { \p _ -> T p Ampersand       }
  \|                            { \p _ -> T p Pipe            }
  \:\=                          { \p _ -> T p Assignment      }
  $digit+                       { \p s -> T p (LitInt (read (C8.unpack s))) }
  $alpha [$alpha $digit \_ \']* { \p s -> T p (Ident s)         }
  @string                       { \p s -> T p (LitStr (C8.tail . C8.init $ s)) }

{

-- The token type:
data TokenTy
    = LitInt Int
    | LitStr ByteString.ByteString
    | Ident  ByteString.ByteString
    | While
    | For
    | To
    | Break
    | Let
    | In
    | End
    | Function
    | Var
    | Type
    | Array
    | If
    | Then
    | Else
    | Do
    | Of
    | Nil
    | Comma
    | Colon
    | OpenParen
    | CloseParen
    | OpenBrace
    | CloseBrace
    | OpenBracket
    | CloseBracket
    | Period
    | PlusSym
    | MulSym
    | SubSym
    | ForwardSlash
    | EqualSym
    | NotEqSym
    | LessThan
    | GreaterThan
    | AtLeast
    | AtMost
    | Ampersand
    | Pipe
    | Assignment
    | Semicolon
    deriving (Eq, Show)

data Token = T 
    { tokenPosn :: AlexPosn
    , tokenTy :: TokenTy
    } deriving (Eq, Show)

main = do
  s <- C8.getContents
  print (alexScanTokens s)

lexFromFile = fmap alexScanTokens . C8.readFile
}
