{-# OPTIONS_GHC -Wall #-}

module Dirichlet (
                 -- * Data/types
                   Options(..) 
                 -- * Main functions
                 , dirichletMass
                 ) where

import Control.Pipe
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Primitive
import System.Random.MWC
import System.Random.MWC.Distributions

-- Data/types ------------------------------------------------------------------

data Options = Options { -- | The Grid over which to associate the Dirichlet
                         --   probabilities.
                         grid  :: (Double, Double)
                         -- | The 'alpha' parameter of the Dirichlet 
                         --   distribution.
                       , alpha :: [Double]                                   }

-- Exported functions ----------------------------------------------------------

dirichletMass :: PrimMonad m => Options -> Gen (PrimState m) 
              -> Producer [([Double], Double)] m ()
dirichletMass opts g0 = do
    let ((b0, b1), a0) = (grid &&& alpha) opts
    zs <- lift $ replicateM (length a0) (replicateM 2 (uniformR (b0, b1) g0))
    vs <- lift $ mapM (\a -> gamma a 1 g0) a0
    yield $ zip zs (map (/ sum vs) vs)

