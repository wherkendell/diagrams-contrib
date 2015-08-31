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

import           Control.Lens         hiding ((#))
import           Control.Monad.State
import           Data.Functor         ((<$>))
import           Data.Hashable
import           Data.Hashable
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import           GHC.Generics

import           Math.MFSolve

import           Diagrams.Coordinates
import           Diagrams.Prelude     (Monoid (..), Monoid', QDiagram, translate, (#))
import           Diagrams.TwoD        (P2, V2, unitX, unitY, unit_X, unit_Y)
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

mkDiaVar :: Handle s -> String -> XY -> Expr (DiaVar s) Double
mkDiaVar h s xy = makeVariable (diaVar h s xy)

mkDiaPVar :: Handle s -> String -> P2 (Expr (DiaVar s) Double)
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
  :: (Hashable v, Ord v)
  => P2 (Expr v Double) -> P2 (Expr v Double) -> System v Double
(coords -> px :& py) =.= (coords -> qx :& qy) = mkSystem [ px === qx, py === qy ]

intro :: QDiagram b V2 Double m -> Constrained s b Double m (Handle s)
intro dia = do
  h <- Handle <$> (handleCounter <+= 1)
  diagrams %= (\m -> m & at h .~ Just dia)
  return h

intros :: [QDiagram b V2 Double m] -> Constrained s b Double m [Handle s]
intros = mapM intro

centerAnchor :: Handle s -> P2 (Expr (DiaVar s) Double)
centerAnchor h = mkDiaPVar h "center"

newAnchorOn
  :: Handle s
  -> (QDiagram b V2 Double m -> P2 Double)
  -> Constrained s b Double m (P2 (Expr (DiaVar s) Double))
newAnchorOn h getP = do
  -- the fromJust is justified, because the type discipline on Handles ensures
  -- they will always represent a valid index in the Map.
  dia <- fromJust <$> use (diagrams . at h)
  let p = getP dia

  v <- varCounter <+= 1
  let anchor = mkDiaPVar h ("a" ++ show v)

  constrain [centerAnchor h .+^ (fmap makeConstant (p .-. origin)) =.= anchor]

  return anchor

-- newAnchor :: Constrained s b Double m (P2 (Expr (DiaVar s) Double))
-- newAnchor = do
--   v <- varCounter <+= 1
--   return $

layout
  :: (Monoid' m, Ord n, Floating n)
  => (forall s. Constrained s b n m a)
  -> QDiagram b V2 n m
layout constr =
  case (s ^. equations) emptyDeps of
    Left depError -> square 1 # fc red     -- XXX
    Right deps    -> mconcat $
      flip map dias $ \(h, dia) ->
        case (getKnown deps (diaVar h "center" X), getKnown deps (diaVar h "center" Y)) of
          (Right xval, Right yval) -> dia # translate (xval ^& yval)
          _ -> square 1 # fc green

  where
    s = execState constr initConstrainedState
    dias = M.assocs (s ^. diagrams)

  -- Solve the system, and then for each diagram in the map, look up
  -- its parameters from the solved system (as appropriate for
  -- scaled/not scaled).  Use the origin for anything unconstrained.
  -- Do scales and translates as appropriate, then mconcat everything.




-- introScaled :: QDiagram b V2 n m -> Constrained b n m Handle
-- introScaled = undefined

  -- like intro, but put (..., True) in the Map, and introduce an
  -- extra radius variable and use it in constraints between center
  -- and outer points


-- vertDirs :: Num n => [(String, V2 n)]
-- vertDirs  = [ ("t", unitY),  ("b", unit_Y) ]
-- horizDirs = [ ("l", unit_X), ("r", unitX)  ]
-- dirs = horizDirs ++ vertDirs ++
--      [ (cv++ch, vv ^+^ vh)
--      | (cv,vv) <- vertDirs, (ch,vh) <- horizDirs
--      ]


