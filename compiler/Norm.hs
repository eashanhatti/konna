{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# OPTIONS_GHC -fdefer-type-errors #-}

module Norm where

import Var
import qualified Core as C
import qualified Data.Map as Map
import Data.List(foldl')
import Data.Maybe(fromJust)
import Debug.Trace
import Control.Monad.Reader
import qualified Data.Set as Set
import GHC.Stack
import Data.List(intersperse)
import Numeric.Natural
import Etc
import Data.Data(Data)

data MetaEntry = Solved Value | Unsolved
  deriving Show

type Metas = Map.Map Global MetaEntry
type Globals = Map.Map Id C.Item
type Locals = [Value]

type Spine = [Value]
data Closure = Closure [Value] C.Term
  deriving (Eq, Ord, Data)

-- Type annotation
type Type = Value

data Value = Value { unVal :: ValueInner }
  deriving (Eq, Ord, Data)

gen = Value

data ValueInner
  = FunIntro Closure Type
  | FunType Type Closure
  | QuoteIntro C.Term Type
  | QuoteType Value
  | IndType Id [Value]
  | IndIntro Id [Value] Type
  | ProdType Id [Value]
  | ProdIntro Type [Value]
  | TypeType0
  | TypeType1
  -- Blocked eliminations
  | StuckFlexVar (Maybe Type) Global Spine
  | StuckGVar Id Type
  | StuckRigidVar Type Level Spine
  | StuckSplice Value
  -- Object-level terms, should only appear under quotes
  | FunElim0 Value Value
  | Var0 Index Value
  | Letrec0 [C.Item] Value
  -- | Let0 Value Value Value
  -- Extras
  | LetrecBound Closure
  | ElabError
  deriving (Eq, Ord, Data)

instance Show Value where
  show (Value v) = case v of
    FunIntro (Closure _ body) ty -> "v{" ++ show body ++ "}"
    FunType inTy (Closure env outTy) -> show inTy ++ " v-> " ++ show outTy ++ " " ++ show env
    QuoteIntro inner _ -> "v<" ++ show inner ++ ">"
    QuoteType innerTy -> "vQuote " ++ show innerTy
    TypeType0 -> "vU0"
    TypeType1 -> "vU1"
    StuckFlexVar _ gl spine -> "v~(?" ++ show (unGlobal gl) ++ " " ++ (concat $ intersperse " " (map show spine)) ++ ")"
    StuckRigidVar ty lv spine -> "v~(" ++ show (unLevel lv) ++ " " ++ (concat $ intersperse " " (map show spine)) ++ "; : " {-++ show ty-} ++ ")"
    FunElim0 lam arg -> "v(" ++ show lam ++ " @ " ++ show arg ++ ")"
    StuckSplice quote -> "v[" ++ show quote ++ "]"
    LetrecBound (Closure _ e) -> "lrb(" ++ show e ++ ")"
    ElabError -> "error"
    StuckGVar nid ty -> "(vg" ++ show nid ++ " : " ++ show ty ++ ")"
    IndType (Id nid) indices -> "vInd" ++ show nid ++ "[" ++ (concat $ intersperse " " (map show indices)) ++ "]"
    IndIntro (Id nid) args _ -> "(v#" ++ show nid ++ (concat $ intersperse " " (map show args)) ++ ")"
    ProdType nid indices -> "vP" ++ show nid ++ "[" ++ (concat $ intersperse " " (map show indices)) ++ "]"
    ProdIntro ty args -> "{" ++ (concat $ intersperse " " (map show args)) ++ "} : " ++ show ty

type Norm a = Reader (Level, Metas, Locals, Globals) a

askLv :: Norm Level
askLv = do
  (lv, _, _, _) <- ask
  pure lv

askMetas :: Norm Metas
askMetas = do
  (_, metas, _, _) <- ask
  pure metas

askLocals :: Norm Locals
askLocals = do
  (_, _, locals, _) <- ask
  pure locals

appClosure :: HasCallStack => Closure -> Value -> Norm Value
appClosure (Closure locals body) val = do
  (level, metas, _, globals) <- ask
  pure $ runReader (eval body) (level, metas, val:locals, globals)

vApp :: HasCallStack => Value -> Value -> Norm Value
vApp (Value lam) arg = case lam of
  FunIntro body vty -> appClosure body arg
  StuckFlexVar vty gl spine -> Value <$> (pure $ StuckFlexVar vty gl (arg:spine))
  StuckRigidVar vty lv spine -> Value <$> (pure $ StuckRigidVar vty lv (arg:spine)) -- FIXME
  _ -> pure $ gen ElabError

vSplice :: HasCallStack => Value -> Norm Value
vSplice val = case unVal val of
  QuoteIntro inner _ -> eval0 inner
  _ -> pure $ gen $ StuckSplice val

vAppSpine :: HasCallStack => Value -> Spine -> Norm Value
vAppSpine val spine = case spine of
  arg:spine -> do
    lam <- vAppSpine val spine
    vApp lam arg
  [] -> pure val

vAppBis :: HasCallStack => Value -> Locals -> [C.BinderInfo] -> Norm Value
vAppBis val locals bis = do
  case (locals, bis) of
    (local:locals, C.Abstract:bis) -> do
      lam <- vAppBis val locals bis
      vApp lam local
    (_:locals, C.Concrete:bis) -> vAppBis val locals bis
    ([], []) -> pure val
    _ -> error ("impossible\n" ++ show locals ++ "\n" ++ show bis ++ "\n" ++ show val)

vMeta :: HasCallStack => Global -> Maybe Type -> Norm Value
vMeta gl vty = do
  metas <- askMetas
  pure $ case fromJust $ Map.lookup gl metas of
    Solved sol -> sol
    Unsolved -> gen $ StuckFlexVar vty gl []

bind :: HasCallStack => Value -> Norm a -> Norm a
bind ty act = do
  (level, metas, locals, globals) <- ask
  pure $ runReader act (incLevel level, metas, (gen $ StuckRigidVar ty level []):locals, globals) -- FIXME

define :: HasCallStack => Value -> Norm a -> Norm a
define val act = do
  (level, metas, locals, globals) <- ask
  pure $ runReader act (incLevel level, metas, val:locals, globals)

defineGlobal :: HasCallStack => C.Item -> Norm a -> Norm a
defineGlobal item act = do
  (level, metas, locals, globals) <- ask
  pure $ runReader act (level, metas, locals, Map.insert (C.itemId item) item globals)

blank :: HasCallStack => Norm a -> Norm a
blank act = do
  (level, metas, locals, globals) <- ask
  pure $ runReader act (incLevel level, metas, (gen ElabError):locals, globals)

blankN :: HasCallStack => Int -> Norm a -> Norm a
blankN n act = case n of
  0 -> act
  n -> blank $ blankN (n - 1) act

index :: HasCallStack => Metas -> Locals -> Globals -> Index -> C.Type -> Int -> Value
index metas locals globals ix ty ix' = case locals of
  [] -> gen ElabError
  x:xs ->
    if ix' == 0 then
      case unVal x of
        LetrecBound (Closure locals' def) -> runReader (eval def) (Level 0, metas, locals', globals)
        _ -> x
    else
      index metas xs globals ix ty (ix' - 1)

eval0 :: HasCallStack => C.Term -> Norm Value
eval0 (C.Term term) = do
  (_, _, locals, _) <- ask
  Value <$> case term of
    C.Var ix ty -> Var0 ix <$> eval0 ty
    C.TypeType0 -> pure TypeType0
    C.FunIntro body ty -> FunIntro (Closure locals body) <$> eval0 ty
    C.FunType inTy outTy -> do
      vInTy <- eval0 inTy
      pure $ FunType vInTy (Closure locals outTy)
    C.FunElim lam arg -> FunElim0 <$> eval0 lam <*> eval0 arg
    C.QuoteElim quote -> unVal <$> (eval quote >>= vSplice)
    C.ProdIntro ty fields -> ProdIntro <$> eval0 ty <*> mapM eval0 fields
    C.Letrec defs body -> Letrec0 defs <$> eval0 body
    C.ElabError -> pure ElabError

eval :: HasCallStack => C.Term -> Norm Value
eval (C.Term term) = do
  (_, metas, locals, globals) <- ask
  case term of
    C.Var ix ty -> pure $ index metas locals globals ix ty (unIndex ix)
    C.TypeType0 -> pure $ Value TypeType0
    C.TypeType1 -> pure $ Value TypeType1
    C.FunIntro body ty -> Value <$> (FunIntro (Closure locals body) <$> eval ty)
    C.FunType inTy outTy -> do
      vInTy <- eval inTy
      pure $ Value $ (FunType vInTy (Closure locals outTy))
    C.FunElim lam arg -> do
      vLam <- eval lam
      vArg <- eval arg
      vApp vLam vArg
    C.QuoteIntro inner ty -> Value <$> (QuoteIntro <$> pure inner <*> eval ty)
    C.QuoteType innerTy -> Value <$> (QuoteType <$> eval innerTy)
    C.QuoteElim quote -> eval quote >>= vSplice
    C.IndIntro cid cds ty -> Value <$> (IndIntro cid <$> mapM eval cds <*> eval ty)
    C.IndType nid indices -> mapM eval indices >>= \is -> pure $ Value $ IndType nid is
    C.ProdType nid indices -> mapM eval indices >>= \is -> pure $ Value $ ProdType nid is
    C.ProdIntro ty fields -> do
      vTy <- eval ty
      vFields <- mapM eval fields
      pure $ Value $ ProdIntro vTy vFields
    C.Letrec defs body -> do
      let withDefs :: Norm a -> [C.Item] -> Norm a
          withDefs act defs = do
            (level, metas, locals, globals) <- ask
            pure $ runReader act (level, metas, locals, foldl' (\gs d -> Map.insert (C.itemId d) d gs) globals defs)
      eval body `withDefs` defs
    C.Meta gl ty -> case fmap eval ty of
      Just ty -> ty >>= \ty -> vMeta gl (Just ty)
      Nothing -> vMeta gl Nothing
    C.InsertedMeta bis gl ty -> case ty of
      Just ty -> eval ty >>= \ty -> vMeta gl (Just ty) >>= \meta -> vAppBis meta locals bis
      Nothing -> vMeta gl Nothing >>= \meta -> vAppBis meta locals bis
    C.GVar nid ty -> case fromJust $ Map.lookup nid globals of
      C.TermDef _ def _ -> eval def
      C.IndDef nid ty -> do
        nTy <- eval ty >>= readback
        eval $ go nTy []
        where
          go :: C.Term -> [C.Term] -> C.Term
          go ty acc = case C.unTerm $ ty of
            C.FunType inTy outTy -> C.gen $ C.FunIntro (go outTy (C.gen (C.Var (Index $ length acc) inTy) : acc)) ty -- FIXME
            C.TypeType1 -> C.gen $ C.IndType nid acc
      C.ConDef nid ty -> do
        nTy <- eval ty >>= readback
        eval $ go nTy []
        where
          go :: C.Term -> [C.Term] -> C.Term
          go ty acc = case C.unTerm $ ty of
            C.FunType inTy outTy -> C.gen $ C.FunIntro (go outTy (C.gen (C.Var (Index $ length acc) inTy) : acc)) ty -- FIXME
            C.IndType tid indices -> C.gen $ C.IndIntro nid acc (C.gen $ C.IndType tid indices)
      C.ProdDef nid ty -> do
        nTy <- eval ty >>= readback
        eval $ go nTy []
        where
          go :: C.Term -> [C.Term] -> C.Term
          go ty acc = case C.unTerm ty of
            C.FunType inTy outTy -> C.gen $ C.FunIntro (go outTy (C.gen (C.Var (Index $ length acc) inTy) : acc)) ty -- FIXME
            C.TypeType0 -> C.gen $ C.ProdType nid acc
      C.SigDef nid ty -> eval ty >>= pure . Value . StuckGVar nid
    C.ElabError -> pure $ Value ElabError

force :: HasCallStack => Value -> Norm Value
force val@(Value val') = do
  metas <- askMetas
  case val' of
    StuckFlexVar vty gl spine | Solved sol <- fromJust $ Map.lookup gl metas -> do
      sol <- vAppSpine sol spine
      force sol
    _ -> pure val

lvToIx :: Level -> Level -> Index
lvToIx lv1 lv2 = Index (unLevel lv1 - unLevel lv2 - 1)

readbackSpine :: HasCallStack => C.Term -> Spine -> Norm C.Term
readbackSpine term@(C.Term term') spine = C.Term <$> do
  case spine of
    arg:spine -> C.FunElim <$> readbackSpine term spine <*> readback arg -- FIXME
    [] -> pure term'

readback0 :: HasCallStack => Value -> Norm C.Term
readback0 (Value val) = C.Term <$> case val of
  TypeType0 -> pure C.TypeType0
  FunElim0 lam arg -> C.FunElim <$> readback0 lam <*> readback0 arg
  Letrec0 defs body -> C.Letrec defs <$> readback0 body
  ProdIntro ty fields -> do
    cTy <- readback0 ty
    cFields <- mapM readback0 fields
    pure $ C.ProdIntro cTy cFields
  Var0 ix ty -> C.Var ix <$> readback0 ty
  StuckSplice quote -> C.QuoteElim <$> readback quote

-- TODO? replace `bind` with `blank`
readback :: HasCallStack => Value -> Norm C.Term
readback val = do
  (Value val) <- force val
  C.Term <$> case val of
    StuckFlexVar vty gl spine -> do
      let
        cty = case vty of
          Just vty -> Just <$> readback vty
          Nothing -> pure Nothing
      meta <- C.Meta <$> pure gl <*> cty
      C.unTerm <$> readbackSpine (C.Term meta) spine
    StuckRigidVar vty lv' spine -> do
      lv <- askLv
      var <- C.Var <$> pure (lvToIx lv lv') <*> readback vty
      C.unTerm <$> readbackSpine (C.Term var) spine
    StuckSplice quote -> C.QuoteElim <$> readback quote
    FunIntro body vty@(unVal -> FunType inTy _) -> do
      lv <- askLv
      vBody <- appClosure body (gen $ StuckRigidVar inTy lv [])
      C.FunIntro <$> blank (readback vBody) <*> readback vty
    FunType inTy outTy@(Closure env tmp) -> do
      lv <- askLv
      vOutTy <- appClosure outTy (gen $ StuckRigidVar inTy lv [])
      C.FunType <$> readback inTy <*> blank (readback vOutTy)
    IndIntro nid cds ty -> C.IndIntro nid <$> mapM readback cds <*> readback ty
    IndType nid indices -> mapM readback indices >>= pure . C.IndType nid
    QuoteIntro inner ty -> C.QuoteIntro <$> (eval0 inner >>= readback) <*> readback ty
    QuoteType innerTy -> C.QuoteType <$> readback innerTy
    ProdType nid indices -> mapM readback indices >>= pure . C.ProdType nid 
    ProdIntro ty fields -> do
      cTy <- readback ty
      cFields <- mapM readback fields
      pure $ C.ProdIntro cTy cFields
    TypeType0 -> pure C.TypeType0
    TypeType1 -> pure C.TypeType1
    StuckGVar nid ty -> readback ty >>= pure . C.GVar nid 
    ElabError -> pure C.ElabError
    _ -> error $ "readback: " ++ show (gen val)