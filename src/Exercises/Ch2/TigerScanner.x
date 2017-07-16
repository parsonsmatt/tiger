{
module Exercises.Ch2.TigerScanner (main) where
}

%wrapper "basic"

$digit = 0-9                      -- digits
$alpha = [a-zA-Z]                -- alphabetic characters

tokens :-

  $white+                       ;
  "//".*                        ;
  let                           { \_ -> Let             }
  in                            { \_ -> In              }
  while                         { \_ -> While           }
  for                           { \_ -> For             }
  to                            { \_ -> To              }
  break                         { \_ -> Break           }
  end                           { \_ -> End             }
  function                      { \_ -> Function        }
  var                           { \_ -> Var             }
  type                          { \_ -> Type            }
  array                         { \_ -> Array           }
  if                            { \_ -> If              }
  then                          { \_ -> Then            }
  else                          { \_ -> Else            }
  do                            { \_ -> Do              }
  of                            { \_ -> Of              }
  nil                           { \_ -> Nil             }
  \,                            { \_ -> Comma           }
  \:                            { \_ -> Colon           }
  \(                            { \_ -> OpenParen       }
  \)                            { \_ -> CloseParen      }
  \[                            { \_ -> OpenBrace       }
  \]                            { \_ -> CloseBrace      }
  \{                            { \_ -> OpenBracket     }
  \}                            { \_ -> CloseBracket    }
  \.                            { \_ -> Period          }
  \+                            { \_ -> PlusSym         }
  \*                            { \_ -> MulSym          }
  \-                            { \_ -> SubSym          }
  \/                            { \_ -> ForwardSlash    }
  \=                            { \_ -> EqualSym        }
  \<\>                          { \_ -> NotEqSym        }
  \<                            { \_ -> LessThan        }
  \>                            { \_ -> GreaterThan     }
  \>\=                          { \_ -> AtLeast         }
  \<\=                          { \_ -> AtMost          }
  \&                            { \_ -> Ampersand       }
  \|                            { \_ -> Pipe            }
  \:\=                          { \_ -> Assignment      }
  $digit+                       { \s -> LitInt (read s) }
  $alpha [$alpha $digit \_ \']* { \s -> Ident s         }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
    = LitInt Int
    | LitStr String
    | Ident String
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
    deriving (Eq, Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
