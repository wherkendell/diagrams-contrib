{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Layout.Constraint
-- Copyright   :  (c) 2015 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- XXX
--
-----------------------------------------------------------------------------

module Diagrams.TwoD.Layout.Constraint where

import           Control.Monad.State
import Control.Lens

import           Math.MFSolve

import           Diagrams.Prelude

type Handle = Int

type System n = Dependencies String n -> Either (DepError n) (Dependencies String n)

data ConstrainedState b v n m = ConstrainedState
  { _varCounter :: Int
  , _equations  :: System n
  , _diagrams   :: M.Map Handle (QDiagram b v n m, Bool)
  }

makeLenses ''ConstrainedState

type Constrained b v n m a = State (ConstrainedState b v n m) a

vertDirs = [ ("t", unitY), ("b", unit_Y) ]
horizDirs = [ ("l", unit_X), ("r", unitX) ]
dirs = horizDirs ++ vertDirs ++ [ (cv++ch, vv ^+^ vh) )| (cv,vv) <- vertDirs, (ch,vh) <- horizDirs ]

intro :: QDiagram b v n m -> Constrained b v n m Handle
intro dia = do
  v <- varCounter <+= 1
  diagrams & at v ?~ (dia, False)
  undefined -- for each direction in dirs, look up envelope and set up
            -- constraint: diagram x/y are fixed distance away from
            -- the given directional points.

intros :: [QDiagram b v n m] -> Constrained b v n m [Handle]
intros = mapM intro

introScaled :: QDiagram b v n m -> Constrained b v n m Handle
introScaled = undefined
  -- like intro, but put (..., True) in the Map, and introduce an
  -- extra radius variable and use it in constraints between center
  -- and outer points

constrain :: System n -> Constrained b v n m ()
constrain = undefined
  -- add a bunch of equations to the system in the state, using some
  -- kind of foldM perhaps

layout :: Constrained b v n m a -> QDiagram b v n m
layout = undefined
  -- Solve the system, and then for each diagram in the map, look up
  -- its parameters from the solved system (as appropriate for
  -- scaled/not scaled).  Use the origin for anything unconstrained.
  -- Do scales and translates as appropriate, then mconcat everything.
