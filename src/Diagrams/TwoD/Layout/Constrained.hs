{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Layout.Constrained
-- Copyright   :  (c) 2015 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- XXX
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Layout.Constrained where

import           Control.Lens         (makeLenses)
import qualified Control.Lens         as L
import           Control.Monad.State
import           Data.Either          (isLeft)
import           Data.Functor         ((<$>))
import           Data.Hashable
import           Data.Hashable
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           GHC.Generics

import qualified Math.MFSolve         as MFS

import           Diagrams.Coordinates
import           Diagrams.Prelude
import           Linear.Affine
import           Linear.Vector

-- XXX
type System v n = MFS.Dependencies v n -> Either (MFS.DepError n) (MFS.Dependencies v n)

-- Don't export DiaID constructor.  The only way you can get a Handle is from
-- intro, so we can guarantee that Handles are always valid
newtype DiaID s = DiaID Int
  deriving (Ord, Eq, Show, Generic)

data VarType = X   -- ^ X-coordinate of a point
             | Y   -- ^ Y-coordinate of a point
             | S   -- ^ scaling factor
             | L   -- ^ length
  deriving (Eq, Ord, Read, Show, Generic)

data Var s = Var (Maybe (DiaID s)) String VarType
  deriving (Eq, Ord, Generic)

instance Hashable (DiaID s)
instance Hashable VarType
instance Hashable (Var s)

var2String :: Var s -> String
var2String (Var h s ty) = show h ++ "_" ++ s ++ "_" ++ show ty

diaVar :: DiaID s -> String -> VarType -> Var s
diaVar d = Var (Just d)

newVar :: String -> VarType -> Var s
newVar = Var Nothing

mkDVar :: Num n => DiaID s -> String -> VarType -> MFS.Expr (Var s) n
mkDVar h s ty = MFS.makeVariable (diaVar h s ty)

mkVar :: Num n => String -> VarType -> MFS.Expr (Var s) n
mkVar s ty = MFS.makeVariable (newVar s ty)

mkDPVar :: Num n => DiaID s -> String -> P2 (MFS.Expr (Var s) n)
mkDPVar h s = mkDVar h s X ^& mkDVar h s Y

mkPVar :: Num n => String -> P2 (MFS.Expr (Var s) n)
mkPVar s = mkVar s X ^& mkVar s Y

-- s is a phantom parameter, used like with ST
data ConstrainedState s b n m = ConstrainedState
  { _equations     :: System (Var s) n
  , _handleCounter :: Int
  , _varCounter    :: Int
  , _diagrams      :: M.Map (DiaID s) (QDiagram b V2 n m)   -- XXX later add Bool for scalable
  }

makeLenses ''ConstrainedState

initConstrainedState :: ConstrainedState s b n m
initConstrainedState = ConstrainedState
  { _equations     = Right
  , _handleCounter = 0
  , _varCounter    = 0
  , _diagrams      = M.empty
  }

type Constrained s b n m a = State (ConstrainedState s b n m) a

mkSystem :: [System v n] -> System v n
mkSystem = flip MFS.solveEqs

constrain :: [System (Var s) n] -> Constrained s b n m ()
constrain sys' =
  equations %= mkSystem . (:sys')

infix 4 =.=
(=.=)
  :: (Hashable v, Ord v, Hashable n, Floating n, RealFrac n)
  => P2 (MFS.Expr v n) -> P2 (MFS.Expr v n) -> System v n
(coords -> px :& py) =.= (coords -> qx :& qy) = mkSystem [ px MFS.=== qx, py MFS.=== qy ]

intro :: QDiagram b V2 n m -> Constrained s b n m (DiaID s)
intro dia = do
  h <- DiaID <$> (handleCounter <+= 1)
  diagrams %= (\m -> m & L.at h .~ Just dia)
  return h

intros :: [QDiagram b V2 n m] -> Constrained s b n m [DiaID s]
intros = mapM intro

centerAnchor :: Num n => DiaID s -> P2 (MFS.Expr (Var s) n)
centerAnchor h = mkDPVar h "center"

centerAnchorX :: Num n => DiaID s -> MFS.Expr (Var s) n
centerAnchorX h = mkDVar h "center" X

centerAnchorY :: Num n => DiaID s -> MFS.Expr (Var s) n
centerAnchorY h = mkDVar h "center" Y

newAnchorOn
  :: (Hashable n, Floating n, RealFrac n)
  => DiaID s
  -> (QDiagram b V2 n m -> P2 n)
  -> Constrained s b n m (P2 (MFS.Expr (Var s) n))
newAnchorOn h getP = do
  -- the fromJust is justified, because the type discipline on DiaIDs ensures
  -- they will always represent a valid index in the Map.
  dia <- fromJust <$> use (diagrams . L.at h)
  let p = getP dia

  v <- varCounter <+= 1
  let anchor = mkDPVar h ("a" ++ show v)

  constrain [centerAnchor h .+^ (fmap MFS.makeConstant (p .-. origin)) =.= anchor]

  return anchor

-- Have to change format of variables, they might not correspond to a diagram,
-- or even to points

-- newAnchor :: Constrained s b n m (P2 (MFS.Expr (Var s) n))
-- newAnchor = do
--   v <- varCounter <+= 1
--   return $

layout
  :: (Monoid' m, Hashable n, Floating n, RealFrac n, Show n)
  => (forall s. Constrained s b n m a)
  -> QDiagram b V2 n m
layout constr =
  case (s ^. equations) MFS.emptyDeps of
    Left depError -> error "depError"
    Right deps    ->
      let deps' = resolve deps
      in  mconcat . flip map dias $ \(h, dia) ->
        case getKnownCenter deps' h of
          [Right xval, Right yval] -> dia # translate (xval ^& yval)
          _ -> error "unknown center"

  -- Check if there are ANY unconstrained center variables.  If there
  -- are, pick one and set it to the origin.  Repeat.

  where
    s = execState constr initConstrainedState
    dias = M.assocs (s ^. diagrams)
    getKnownCenter deps h
      = [MFS.getKnown deps (diaVar h "center" X), MFS.getKnown deps (diaVar h "center" Y)]
    resolve deps =
      let diaVars = map fst dias >>= \h -> map (h,) (getKnownCenter deps h)
      in  case filter (isLeft.snd) diaVars of
            [] -> deps
            ((h1,_):_) -> resolve (either (error . show) id (MFS.solveEqs deps [h1 MFS.=== 0]))

constrainWith
  :: (Hashable n, RealFrac n, Floating n, Ord n, Monoid' m)
  => -- (forall a. (...) => [a] -> a)
     ([[Located (Envelope V2 n)]] -> [Located (Envelope V2 n)])
  -> [DiaID s]
  -> Constrained s b n m ()
constrainWith f hs = do
  diaMap <- use diagrams
  let dias  = map (fromJust . flip M.lookup diaMap) hs
      envs  = map ((:[]) . (`at` origin) . getEnvelope) dias
      envs' = f envs
      ps    = map loc envs'
      locHs = zip hs ps
  zipWithM_
    (\(h1,q1) (h2,q2) ->
      constrain [centerAnchor h1 .+^ fmap MFS.makeConstant (q2 .-. q1) =.= centerAnchor h2]
    )
    locHs (tail locHs)

sameX :: (Hashable n, Floating n, RealFrac n) => DiaID s -> DiaID s -> Constrained s b n m ()
sameX h1 h2 = constrain [centerAnchorX h1 MFS.=== centerAnchorX h2]

sameY :: (Hashable n, Floating n, RealFrac n) => DiaID s -> DiaID s -> Constrained s b n m ()
sameY h1 h2 = constrain [centerAnchorY h1 MFS.=== centerAnchorY h2]

{- TODO:

   - introduce scaling as an extra constraint var
     (Introduce it for all diagrams, but for fixed ones just constrain it to be 1.
     Then look it up and use it for all diagrams, defaulting to 1.)
   - Ways to use juxtaposition etc. combinators
   - Specialize newAnchorOn for common cases (envelope, trace)
   - shorter name for centerAnchor
   - better name for intro
-}

-- introScaled :: QDiagram b V2 n m -> Constrained b n m DiaID
-- introScaled = undefined

  -- like intro, but put (..., True) in the Map, and introduce an
  -- extra radius variable and use it in constraints between center
  -- and outer points
