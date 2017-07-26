-- vim: ft=happy
{
{-# GHC_OPTIONS -fno-warn #-}
module Exercises.Ch3.TigerParser 
    ( parse
    , module X
    ) where

import Exercises.Ch2.TigerScanner (Token(..), TokenTy)
import qualified Exercises.Ch2.TigerScanner as Tok
import Data.ByteString.Lazy (ByteString)
import Data.String
import Language.Tiger.Syntax as X
import Control.Monad.Except
}

%name parse
%tokentype { Token }
%monad { Either String } { (>>=) } { return }

%token
    let      { T _ Tok.Let          }
    in       { T _ Tok.In           }
    while    { T _ Tok.While        }
    for      { T _ Tok.For          }
    to       { T _ Tok.To           }
    break    { T _ Tok.Break        }
    end      { T _ Tok.End          }
    function { T _ Tok.Function     }
    var      { T _ Tok.Var          }
    type     { T _ Tok.Type         }
    array    { T _ Tok.Array        }
    if       { T _ Tok.If           }
    then     { T _ Tok.Then         }
    else     { T _ Tok.Else         }
    do       { T _ Tok.Do           }
    of       { T _ Tok.Of           }
    nil      { T _ Tok.Nil          }
    '='      { T _ Tok.EqualSym     }
    '{'      { T _ Tok.OpenBracket  }
    '}'      { T _ Tok.CloseBracket }
    '('      { T _ Tok.OpenParen    }
    ')'      { T _ Tok.CloseParen   }
    ','      { T _ Tok.Comma        }
    ':'      { T _ Tok.Colon        }
    ':='     { T _ Tok.Assignment   }
    '.'      { T _ Tok.Period       }
    '['      { T _ Tok.OpenBrace    }
    ']'      { T _ Tok.CloseBrace   }
    ';'      { T _ Tok.Semicolon    }
    '-'      { T _ Tok.SubSym       }
    '+' { T _ Tok.PlusSym }
    '*' { T _ Tok.MulSym }
    '/' { T _ Tok.ForwardSlash }
    '<>' { T _ Tok.NotEqSym }
    '<'  { T _ Tok.LessThan }
    '>'  { T _ Tok.GreaterThan }
    '<=' { T _ Tok.AtMost }
    '>=' { T _ Tok.AtLeast }
    '&' { T _ Tok.Ampersand }
    '|' { T _ Tok.Pipe }

    int   { T _ (Tok.LitInt $$) }
    str   { T _ (Tok.LitStr $$) }
    ident { T _ (Tok.Ident $$)  }

%right in
%left '|'
%left '&'
%nonassoc '<' '>' '<>' '=' '<=' '>='
%left '+' '-'
%left '*' '/'

%%

Exprs :: { [Expr] }
Exprs : {- empty -}     { []      }
      |  Expr Exprs1 { $1 : $2 }

Exprs1 :: { [Expr] }
Exprs1 : {- empty -} { [] }
       | ';' Expr Exprs1 { $2 : $3 }

Expr :: { Expr }
Expr : let Decs in Exprs end { ELet $2 (ESeq $4) }
     | if Expr then Expr MElse { case $5 of
            Nothing -> EIf $2 $4 
            Just e  -> EIfElse $2 $4 e }
     | while Expr do Expr { EWhile $2 $4 }
     | for ident ':=' Expr to Expr do Expr { EFor EscYes (Ident $2) $4 $6 $8 }
     | '-' Expr  { BinOp (EInt 0) (ArithOp OMinus) $2 }
     | Term { $1 }

Term :: { Expr }
Term : Term BinOp Term { case $2 of
        Left b ->
            case b of
                OOr -> EIfElse $1 (EInt 1) $3
                OAnd -> EIfElse $1 $3 (EInt 0)
        Right a -> 
            BinOp $1 a $3 
        }
     | break { EBreak }
     | int { EInt $1 }
     | str { EStr $1 }
     | nil { ENil }
     | ident IdentFollower  { $2 $1 }
     | '(' Paren1 { $2 }

Paren1 :: { Expr }
Paren1 : Exprs ')' { ESeq $1 }
       | ')'       { EUnit }

IdentFollower :: { ByteString -> Expr }
IdentFollower : LFollower { \i -> $1 (LIdent (Ident i)) }
              | '(' FunArgs ')' { \i -> FunCall (Ident i) $2 }
              | '[' Expr ']' of Expr { \i -> EArrCreate (Ident i) $2 $5  }
              | '{' RecFields '}' { \i -> ERecCreate (Ident i) $2 }

RecFields :: { [RecAssn] }
RecFields : {- empty -} { [] }
          | RecAssn RecFields1 { $1 : $2 }

RecAssn :: { RecAssn }
RecAssn : ident '=' Expr { RecAssn (Ident $1) $3 }

RecFields1 :: { [RecAssn] }
RecFields1 : {- empty -} { [] }
           | ',' RecAssn RecFields1 { $2 : $3 }

LFollower :: { LValue -> Expr }
LFollower : {- -} { \l -> ELValue l }
          | '.' ident LFollower { \l -> $3 (LDot l (Ident $2)) }
          | '[' Expr ']' LFollower { \l -> $4 (LSubscript l $2) }
          | ':=' Expr { \l -> EAssign l $2 }

FunArgs :: { [Expr] }
FunArgs : {- empty -} { [] }
        | Expr FunArgs1 { $1 : $2 }

FunArgs1 :: { [Expr] }
FunArgs1 : {- empty -}       { [] }
         | ',' Expr FunArgs1 { $2 : $3 }

Decs :: { [Decl] }
Decs : {- empty -} { []      }
     | Decs1 Dec    { $2 : $1 }

Decs1 :: { [Decl] }
Decs1 : {- empty -} { [] }
      | Decs1 Dec { $2 : $1 }

Dec :: { Decl }
Dec : TypeDec  { $1  }
    | VarDec   { VarDec $1 }
    | FunDec   { FunDec $1 }

TypeDec :: { Decl }
TypeDec : type ident '=' Type { TyDec (Ident $2) $4 }

Type :: { TypeP }
Type : ident          { TypeName (Ident $1)  }
     | TyFieldsBraces { TypeFields $1        }
     | array of ident { TypeArray (Ident $3) }

TyFieldsParens :: { [TypeField] }
TyFieldsParens : '(' ')' { [] }
               | '(' TyField TyFields1 ')' { $2 : $3 }

TyFieldsBraces :: { [TypeField] }
TyFieldsBraces : '{' '}'                   { []      }
               | '{' TyField TyFields1 '}' { $2 : $3 }

TyFields1 :: { [TypeField] }
TyFields1 : {- empty -}           { []      }
          | ',' TyField TyFields1 { $2 : $3 }

TyField :: { TypeField }
TyField : ident ':' ident { TypeField (Ident $1) (Ident $3) }

VarDec :: { VarDecl }
VarDec : var ident OptAnn ':=' Expr { mkVarDecl (Ident $2) $3 $5 }

FunDec :: { FunDecl }
FunDec : function ident TyFieldsParens OptAnn '=' Expr { case $4 of
       Just ty -> Func (Ident $2) $3 ty $6 
       Nothing -> Proc (Ident $2) $3 $6 }

OptAnn :: { Maybe Ident }
OptAnn : {- empty -} { Nothing             }
       | ':' ident   { Just (Ident $2) }

MElse :: { Maybe Expr }
MElse :           { Nothing }
      | else Expr { Just $2 }
    
BinOp :: { Either BoolOp Op }
BinOp : '+' { Right $ ArithOp OPlus }
      | '-' { Right $ ArithOp OMinus }
      | '*' { Right $ ArithOp OTimes }
      | '/' { Right $ ArithOp ODivide }
      | '=' { Right $ CompOp OEQ }
      | '<>' { Right $ CompOp ONEQ }
      | '<'  { Right $ CompOp OLT }
      | '>'  { Right $ CompOp OGT }
      | '<=' { Right $ CompOp OLTE }
      | '>=' { Right $ CompOp OGTE }
      | '&' { Left OAnd }
      | '|' { Left OOr }

{

happyError = parseError

parseError :: [Token] -> Either String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"
}
