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

import           Control.Lens
import           Control.Monad.State
import           Data.Hashable
import qualified Data.Map             as M

import           Math.MFSolve

import           Diagrams.Coordinates
import           Diagrams.Prelude     (QDiagram)
import           Diagrams.TwoD        (P2, V2, unitX, unitY, unit_X, unit_Y)
import           Linear.Affine
import           Linear.Vector

type Handle = Int

type System n = Dependencies SimpleVar n -> Either (DepError n) (Dependencies SimpleVar n)

data ConstrainedState b v n m = ConstrainedState
  { _varCounter :: Int
  , _equations  :: System n
  , _diagrams   :: M.Map Handle (QDiagram b v n m, Bool)
  }

makeLenses ''ConstrainedState

type Constrained b v n m a = State (ConstrainedState b v n m) a

vertDirs :: Num n => [(String, V2 n)]
vertDirs  = [ ("t", unitY),  ("b", unit_Y) ]
horizDirs = [ ("l", unit_X), ("r", unitX)  ]
dirs = horizDirs ++ vertDirs ++
     [ (cv++ch, vv ^+^ vh)
     | (cv,vv) <- vertDirs, (ch,vh) <- horizDirs
     ]

mkSystem :: [System n] -> System n
mkSystem = flip solveEqs

-- (=.=) :: (Hashable n, RealFrac (Phase n))
--       => P2 (Expr SimpleVar n) -> P2 (Expr SimpleVar n) -> System n
-- can't use the above since Phase is not exported by mfsolve

infix 4 =.=
(=.=) :: P2 (Expr SimpleVar Double) -> P2 (Expr SimpleVar Double) -> System Double
(coords -> px :& py) =.= (coords -> qx :& qy) = mkSystem [ px === qx, py === qy ]

intro :: QDiagram b v Double m -> Constrained b v Double m Handle
intro dia = do
  v <- varCounter <+= 1
  diagrams %= (\m -> m & at v .~ Just (dia, False))
  forM_ dirs $ \(dirName, dirV) ->
    constrain $ ccenter v .+^ dirV =.= subpoint dirName v

  undefined -- for each direction in dirs, look up envelope and set up
            -- constraint: diagram x/y are fixed distance away from
            -- the given directional points.

subvar :: String -> Handle -> Expr SimpleVar Double
subvar s h = makeVariable . SimpleVar $ show h ++ "_" ++ s

-- TODO: use a data type for these e.g. var =~ (Handle, Name, XY)

subpoint :: String -> Handle -> P2 (Expr SimpleVar Double)
subpoint s h = subvar (s ++ "_x") h ^& subvar (s ++ "_y") h

cx :: Handle -> Expr SimpleVar Double
cx = subvar "center_x"

cy :: Handle -> Expr SimpleVar Double
cy = subvar "center_y"

ccenter :: Handle -> P2 (Expr SimpleVar Double)
ccenter h = cx h ^& cy h

intros :: [QDiagram b v Double m] -> Constrained b v Double m [Handle]
intros = mapM intro

introScaled :: QDiagram b v n m -> Constrained b v n m Handle
introScaled = undefined
  -- like intro, but put (..., True) in the Map, and introduce an
  -- extra radius variable and use it in constraints between center
  -- and outer points

constrain :: System n -> Constrained b v n m ()
constrain sys' =
  equations %= mkSystem . (\sys -> [sys,sys'])

layout :: Constrained b v n m a -> QDiagram b v n m
layout = undefined
  -- Solve the system, and then for each diagram in the map, look up
  -- its parameters from the solved system (as appropriate for
  -- scaled/not scaled).  Use the origin for anything unconstrained.
  -- Do scales and translates as appropriate, then mconcat everything.
