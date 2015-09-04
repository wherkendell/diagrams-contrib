{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Data.Either (isLeft)
import           Control.Lens         (makeLenses)
import qualified Control.Lens as L
import           Control.Monad.State
import           Data.Functor         ((<$>))
import           Data.Hashable
import           Data.Hashable
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           GHC.Generics

import           Math.MFSolve

import           Diagrams.Coordinates
import           Diagrams.Prelude     hiding ((===))
import           Linear.Affine
import           Linear.Vector

type System v n = Dependencies v n -> Either (DepError n) (Dependencies v n)

-- XXX change name 'Handle' to something better

-- Don't export Handle constructor.  The only way you can get a Handle is from
-- intro, so we can guarantee that Handles are always valid
newtype Handle s = Handle Int
  deriving (Ord, Eq, Show, Generic)

data XY = X | Y
  deriving (Eq, Ord, Read, Show, Generic)
data DiaVar s = DiaVar (Handle s) String XY  -- XXX some vars might not be associated with a diagram!
  deriving (Eq, Ord, Generic)

instance Hashable (Handle s)
instance Hashable XY
instance Hashable (DiaVar s)

diaVar2String :: DiaVar s -> String
diaVar2String (DiaVar h s xy) = show h ++ "_" ++ s ++ "_" ++ show xy

diaVar :: Handle s -> String -> XY -> DiaVar s
diaVar = DiaVar

mkDiaVar :: Num n => Handle s -> String -> XY -> Expr (DiaVar s) n
mkDiaVar h s xy = makeVariable (diaVar h s xy)

mkDiaPVar :: Num n => Handle s -> String -> P2 (Expr (DiaVar s) n)
mkDiaPVar h s = mkDiaVar h s X ^& mkDiaVar h s Y

-- s is a phantom parameter, used like with ST
data ConstrainedState s b n m = ConstrainedState
  { _equations     :: System (DiaVar s) n
  , _handleCounter :: Int
  , _varCounter    :: Int
  , _diagrams      :: M.Map (Handle s) (QDiagram b V2 n m)   -- XXX later add Bool for scalable
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
mkSystem = flip solveEqs

constrain :: [System (DiaVar s) n] -> Constrained s b n m ()
constrain sys' =
  equations %= mkSystem . (:sys')

-- (=.=) :: (Hashable n, RealFrac (Phase n))
--       => P2 (Expr SimpleVar n) -> P2 (Expr SimpleVar n) -> System n
-- can't use the above since Phase is not exported by mfsolve

infix 4 =.=
(=.=)
  :: (Hashable v, Ord v, Hashable n, Floating n, RealFrac n)
  => P2 (Expr v n) -> P2 (Expr v n) -> System v n
(coords -> px :& py) =.= (coords -> qx :& qy) = mkSystem [ px === qx, py === qy ]

intro :: QDiagram b V2 n m -> Constrained s b n m (Handle s)
intro dia = do
  h <- Handle <$> (handleCounter <+= 1)
  diagrams %= (\m -> m & L.at h .~ Just dia)
  return h

intros :: [QDiagram b V2 n m] -> Constrained s b n m [Handle s]
intros = mapM intro

centerAnchor :: Num n => Handle s -> P2 (Expr (DiaVar s) n)
centerAnchor h = mkDiaPVar h "center"

centerAnchorX :: Num n => Handle s -> Expr (DiaVar s) n
centerAnchorX h = mkDiaVar h "center" X

centerAnchorY :: Num n => Handle s -> Expr (DiaVar s) n
centerAnchorY h = mkDiaVar h "center" Y

newAnchorOn
  :: (Hashable n, Floating n, RealFrac n)
  => Handle s
  -> (QDiagram b V2 n m -> P2 n)
  -> Constrained s b n m (P2 (Expr (DiaVar s) n))
newAnchorOn h getP = do
  -- the fromJust is justified, because the type discipline on Handles ensures
  -- they will always represent a valid index in the Map.
  dia <- fromJust <$> use (diagrams . L.at h)
  let p = getP dia

  v <- varCounter <+= 1
  let anchor = mkDiaPVar h ("a" ++ show v)

  constrain [centerAnchor h .+^ (fmap makeConstant (p .-. origin)) =.= anchor]

  return anchor

-- newAnchor :: Constrained s b n m (P2 (Expr (DiaVar s) n))
-- newAnchor = do
--   v <- varCounter <+= 1
--   return $

layout
  :: (Monoid' m, Hashable n, Floating n, RealFrac n, Show n)
  => (forall s. Constrained s b n m a)
  -> QDiagram b V2 n m
layout constr =
  case (s ^. equations) emptyDeps of
    Left depError -> error "depError"
    Right deps    ->
      let deps' = resolve deps dias
      in  mconcat . flip map dias $ \(h, dia) ->
        case getKnownCenter deps' h of
          (Right xval, Right yval) -> dia # translate (xval ^& yval)
          _ -> error "unknown center"

  -- Check if there are ANY unconstrained center variables.  If there
  -- are, pick one and set it to the origin.  Repeat.

  where
    s = execState constr initConstrainedState
    dias = M.assocs (s ^. diagrams)
    getKnownCenter deps h
      = (getKnown deps (diaVar h "center" X), getKnown deps (diaVar h "center" Y))
    resolve deps dias =
      -- XXX do something with half-constrained points.
      -- Partition into constrained, half-constrained, unconstrained.  Prefer fixing
      -- half-constrained first.
      case flip filter dias (\(h, _) -> let (x,y) = getKnownCenter deps h in isLeft x || isLeft y) of
        [] -> deps
        ((h1,_):_) -> resolve (either (error . show) id (solveEqs deps [centerAnchor h1 =.= origin])) dias


constrainWith
  :: (Hashable n, RealFrac n, Floating n, Ord n, Monoid' m)
  => -- (forall a. (...) => [a] -> a)
     ([[Located (Envelope V2 n)]] -> [Located (Envelope V2 n)])
  -> [Handle s]
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
      constrain [centerAnchor h1 .+^ fmap makeConstant (q2 .-. q1) =.= centerAnchor h2]
    )
    locHs (tail locHs)

sameX :: (Hashable n, Floating n, RealFrac n) => Handle s -> Handle s -> Constrained s b n m ()
sameX h1 h2 = constrain [centerAnchorX h1 === centerAnchorX h2]

sameY :: (Hashable n, Floating n, RealFrac n) => Handle s -> Handle s -> Constrained s b n m ()
sameY h1 h2 = constrain [centerAnchorY h1 === centerAnchorY h2]

{- TODO:

   - introduce scaling as an extra constraint var
     (Introduce it for all diagrams, but for fixed ones just constrain it to be 1.
     Then look it up and use it for all diagrams, defaulting to 1.)
   - Ways to use juxtaposition etc. combinators
   - Specialize newAnchorOn for common cases (envelope, trace)
   - shorter name for centerAnchor
   - better name for intro
-}

-- introScaled :: QDiagram b V2 n m -> Constrained b n m Handle
-- introScaled = undefined

  -- like intro, but put (..., True) in the Map, and introduce an
  -- extra radius variable and use it in constraints between center
  -- and outer points
