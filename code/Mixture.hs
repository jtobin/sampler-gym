module Mixture where

import PolyaUrn
import Control.Pipe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Primitive
import Data.List
import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Float
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.IO

-- | Density function for an isotropic Gaussian.  The (identity) covariance 
--   matrix is multiplied by the scalar 'sig'.
isoGauss :: [Double] -> Double -> [Double] -> Double
isoGauss mu sig xs = foldl1' (*) (zipWith density nds xs)
    where nds = map (`normalDistr` sig) mu
{-# INLINE isoGauss #-}

-- prop_isoGaussBehavesLikeIThinkItDoes

massTransformer :: (Ord b, Fractional b, Monad m) => Pipe Urn [([Double], b)] m r
massTransformer = forever $ do
    urn <- await
    yield . sortBy (flip compare `on` snd) $ 
        zip (HashMap.keys urn) (massFunction urn)
 
gaussMixer :: Double 
           -> Pipe [([Double], Double)] ([Double] -> Double) IO ()
gaussMixer sig = forever $ do
    mass <- await
    lift $ hPutStrLn stderr "dirichlet process sample: " 
    lift $ hPrint stderr mass
    let logMixtureDensity xs = log $ sum $ zipWith (*) (map snd mass) 
          (map ((\m -> isoGauss m sig xs) . fst) mass)
    yield logMixtureDensity
 

    
