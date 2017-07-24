{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Exercises.Ch5.Semantic where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable             (for_)
import           Data.IORef
import           Data.Map                  (Map)
import           Data.Traversable          (for)

import           Exercises.Ch3.TigerParser

newtype Unique = Unique { unUnique :: Int }
    deriving (Eq, Ord)

nextUnique :: Unique -> Unique
nextUnique = Unique . succ . unUnique

data Symbol = Symbol Unique Ident

instance Eq Symbol where
    Symbol a _ == Symbol b _ =
        a == b

instance Ord Symbol where
    Symbol a _ `compare` Symbol b _ =
        a `compare` b

data Type
    = TyInt
    | TyStr
    | TyRec Unique [(Symbol, Type)]
    | TyArr Unique Type
    | TyNil
    | TyUnit
    | TyName Symbol (IORef (Maybe Type))
    deriving Eq

type TypeEnv = Map Symbol Type
type ValueEnv = Map Symbol EnvEntry
type SymbolEnv = Map Ident Symbol

data TypeError
    = TypeMismatch Type Type Expr

type TypeChecker = ExceptT TypeError (StateT TyCheckEnv IO)

data TyCheckEnv
    = TyCheckEnv
    { _typeEnv       :: TypeEnv
    , _valueEnv      :: ValueEnv
    , _currentUnique :: Unique
    , _symbolEnv     :: SymbolEnv
    }

data EnvEntry
    = VarEntry Type
    | FuncEntry [Type] Type

makeLenses ''TyCheckEnv

fresh :: TypeChecker Unique
fresh = currentUnique <<%= nextUnique

mkSym :: Ident -> TypeChecker Symbol
mkSym i = do
    msym <- use (symbolEnv . at i)
    case msym of
        Just a ->
            pure a
        Nothing -> do
            unique <- fresh
            let s = Symbol unique i
            symbolEnv . at i .= Just s
            pure s

addType :: Ident -> Type -> TypeChecker ()
addType i t = do
    s <- mkSym i
    typeEnv . at s .= Just t

initializeBaseTypeEnv :: TypeChecker ()
initializeBaseTypeEnv = do
    addType "str" TyStr
    addType "int" TyInt
    addType "nil" TyNil
    addType "unit" TyUnit

typeMismatch :: Type -> Type -> Expr -> TypeChecker a
typeMismatch expected actual expr =
    throwError $ TypeMismatch expected actual expr

checkType :: Type -> Type -> Expr -> TypeChecker Type
checkType expected actual expr = do
    when (expected /= actual) (typeMismatch expected actual expr)
    pure expected

scoped :: TypeChecker a -> TypeChecker a
scoped action = do
    old <- get
    res <- action
    put old
    pure res

-- local, for state
within :: (TyCheckEnv -> TyCheckEnv) -> TypeChecker a -> TypeChecker a
within f ma =
    scoped (modify f *> ma)

typeCheckExpr :: Expr -> TypeChecker Type
typeCheckExpr = \case
    BinOp e1 op' e2 ->
        case op' of
            ArithOp _ -> do
                t1 <- typeCheckExpr e1
                _ <- checkType TyInt t1 e1
                t2 <- typeCheckExpr e2
                checkType TyInt t2 e2
            CompOp _ -> do
                t1 <- typeCheckExpr e1
                t2 <- typeCheckExpr e2
                checkType t1 t2 (BinOp e1 op' e2)
    ELet decs expr ->
        scoped $ do
            for_ decs typeCheckDecl
            typeCheckExpr expr

setAndReturnType :: Symbol -> Type -> TypeChecker Type
setAndReturnType s t = do
    typeEnv . at s .= Just t
    pure t

typeCheckDecl :: Decl -> TypeChecker Type
typeCheckDecl = \case
    TyDec i typeDec -> do
        s <- mkSym i
        ty <- case typeDec of
            TypeName i' -> do
                s' <- mkSym i'
                mtype <- use (typeEnv . at s')
                case mtype of
                    Nothing -> do
                        ref <- liftIO $ newIORef Nothing
                        pure $ TyName s' ref
                    Just typ ->
                        case typ of
                            TyName _ ref -> do
                                mtype' <- liftIO $ readIORef ref
                                case mtype' of
                                    Nothing ->
                                        pure $ TyName s' ref
                                    Just refTyp -> do
                                        typeEnv . at s' .= Just refTyp
                                        pure refTyp
                            _ ->
                                pure typ

            TypeFields tyFields -> do
                pairs <- for tyFields mkFieldPair
                u <- fresh
                pure $ TyRec u pairs

            TypeArray ident -> do
                u <- fresh
                s' <- mkSym ident
                TyArr u <$> lookupTyOrName s'
        s `setAndReturnType` ty
    VarDec (VarDecl _ ident optSig expr) -> do
        s <- mkSym ident
        exprTyp <- typeCheckExpr expr
        sigType <- case optSig of
            Nothing ->
                pure exprTyp
            Just sig -> do
                sigSym <- mkSym sig
                mtype <- use (typeEnv . at sigSym)
                case mtype of
                    Nothing -> do
                        typeEnv . at sigSym .= Just exprTyp
                        pure exprTyp
                    Just typ ->
                        case typ of
                            TyName _ ref -> do
                                mtyp' <- liftIO (readIORef ref)
                                case mtyp' of
                                    Nothing -> do
                                        liftIO (writeIORef ref (Just exprTyp))
                                        pure exprTyp
                                    Just refTyp ->
                                        pure refTyp
                            _ ->
                                if typ == exprTyp
                                   then pure typ
                                   else typeMismatch typ exprTyp expr
        s `setAndReturnType` sigType

    FunDec funDecl ->
        case funDecl of
            Func ident tyFields retTyp expr ->
                error "finish me"
            Proc ident tyFields expr -> do
                typ <- scoped $ do
                    for_ tyFields $ \(TypeField var typ) ->
                        error "add these bindings to the scope"

                    exprTy <- typeCheckExpr expr
                    unless (exprTy == TyUnit) $
                        typeMismatch TyUnit exprTy expr
                    pure exprTy
                sym <- mkSym ident
                sym `setAndReturnType` typ





lookupTyOrName :: Symbol -> TypeChecker Type
lookupTyOrName s = do
    mty <- use (typeEnv . at s)
    case mty of
        Just ty ->
            case ty of
                TyName s' ref -> do
                    mty' <- liftIO (readIORef ref)
                    case mty' of
                        Nothing ->
                            pure ty
                        Just ty' -> do
                            typeEnv . at s' .= Just ty'
                            pure ty'
                _ ->
                    pure ty
        Nothing -> do
            ref <- liftIO (newIORef Nothing)
            pure (TyName s ref)


mkFieldPair :: TypeField -> TypeChecker (Symbol, Type)
mkFieldPair (TypeField fieldName typeName) = do
    fieldSym <- mkSym fieldName
    typeSym <- mkSym typeName
    ty <- lookupTyOrName typeSym
    pure (fieldSym, ty)

