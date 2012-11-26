{-# OPTIONS_GHC -Wall #-}

module FlatSampler (flatMcmcSampler, Options(..), MarkovChain(..)) where

import Numeric.MCMC.Flat (runChain, MarkovChain(..), Options(..))
import Control.Pipe
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import System.Random.MWC

-- Exported functions ----------------------------------------------------------

flatMcmcSampler :: Options
                -> MarkovChain
                -> Gen RealWorld 
                -> Consumer ([Double] -> Double) IO r
flatMcmcSampler opts initState g = forever $ do
    target <- await
    lift $ runChain target opts initState g

