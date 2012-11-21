module Sampler where

import PolyaUrn
import Numeric.MCMC.Flat (runChain, MarkovChain(..), Ensemble(..))
import qualified Numeric.MCMC.Flat as Flat
import qualified Data.Vector as V
import System.Random.MWC
import Control.Pipe
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans

sampler :: Int -> Int -> Options -> Gen RealWorld -> Consumer ([Double] -> Double) IO r
sampler nepochs nparticles opts g = forever $ do
    target <- await
    let area = (\(Grid g0 g1) -> g1) $ grid opts 
    starts <- lift $ replicateM nparticles (replicateM 2 (uniformR (0 :: Double, area) g))
    let inits  = V.fromList starts 
        params = Flat.Options target (V.length inits) 25
        config = MarkovChain inits 0

    lift $ runChain params nepochs 0 1 config g




