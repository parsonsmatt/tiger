-- vim: ft=happy
{
module Exercises.Ch3.TigerParser where

import Exercises.Ch2.TigerScanner (Token(..), TokenTy)
import qualified Exercises.Ch2.TigerScanner as Tok
import Data.ByteString.Lazy (ByteString)
}

%name parse
%tokentype { Token }

%token
    let      { T _ Tok.Let      }
    in       { T _ Tok.In       }
    while    { T _ Tok.While    }
    for      { T _ Tok.For      }
    to       { T _ Tok.To       }
    break    { T _ Tok.Break    }
    end      { T _ Tok.End      }
    function { T _ Tok.Function }
    var      { T _ Tok.Var      }
    type     { T _ Tok.Type     }
    array    { T _ Tok.Array    }
    if       { T _ Tok.If       }
    then     { T _ Tok.Then     }
    else     { T _ Tok.Else     }
    do       { T _ Tok.Do       }
    of       { T _ Tok.Of       }
    nil      { T _ Tok.Nil      }
    '='      { T _ Tok.EqualSym }
    '{' { T _ Tok.OpenBracket }
    '}' { T _ Tok.CloseBracket  }
    ',' { T _ Tok.Comma }
    ':' { T _ Tok.Colon }

    int   { T _ (Tok.LitInt $$) }
    str   { T _ (Tok.LitStr $$) }
    ident { T _ (Tok.Ident $$)  }

%%

Decs :: { [Decl] }
Decs : {- empty -} { []      }
     | Decs Dec    { $2 : $1 }

Dec :: { Decl }
Dec : TypeDec  { TyDec $1  }
    | VarDec   { VarDec  }
    | FunDec   { FunDec  }

TypeDec :: { TypeDecl }
TypeDec : type ident '=' Type { TypeDecl (TypeIdent (Ident $2)) $4 }

Type :: { Type }
Type : ident          { TypeName (TypeIdent (Ident $1)) }
     | TyFields       { TypeFields $1                   }
     | array of ident { TypeArray (Ident $3)            }

TyFields :: { [TypeField] }
TyFields : {- empty -}               { []      }
         | '{' TyField TyFields1 '}' { $2 : $3 }

TyFields1 :: { [TypeField] }
TyFields1 : {- empty -}           { []      }
          | ',' TyField TyFields1 { $2 : $3 }

TyField :: { TypeField }
TyField : ident ':' ident { TypeField (Ident $1) (TypeIdent (Ident $3)) }

VarDec : {- -} {}
FunDec : {- -} {}

{

happyError _ = error "parse error"

newtype Ident = Ident { unIdent :: ByteString }
    deriving (Eq, Show)

newtype TypeIdent = TypeIdent { unTypeIdent :: Ident }
    deriving (Eq, Show)

data Decl
    = TyDec TypeDecl
    | VarDec 
    | FunDec

data TypeDecl = TypeDecl TypeIdent Type

data Type
    = TypeName TypeIdent
    | TypeFields [TypeField]
    | TypeArray Ident

data TypeField
    = TypeField Ident TypeIdent

}

