{-# OPTIONS_GHC -Wall #-}

module Mixture where

import PolyaUrn
import Control.Pipe
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Function
import qualified Data.HashMap.Strict as HashMap
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.IO

import Test.QuickCheck

-- | Density function for an isotropic Gaussian.  The (identity) covariance 
--   matrix is multiplied by the scalar 'sig'.
isoGauss :: [Double] -> Double -> [Double] -> Double
isoGauss mu sig xs = foldl1' (*) (zipWith density nds xs)
    where nds = map (`normalDistr` sig) mu
{-# INLINE isoGauss #-}

massTransformer :: (Ord b, Fractional b, Monad m) 
                => Pipe Urn [([Double], b)] m r
massTransformer = forever $ do
    urn <- await
    yield . sortBy (flip compare `on` snd) $ 
        zip (HashMap.keys urn) (massFunction urn)
 
gaussMixer :: Double 
           -> Pipe [([Double], Double)] ([Double] -> Double) IO ()
gaussMixer sig = forever $ do
    mass <- await
    lift $ hPutStrLn stderr "Dirichlet process sample: " 
    lift $ hPrint stderr mass
    let logMixtureDensity xs = log $ gaussianMixture xs mass sig 
    yield logMixtureDensity
 
gaussianMixture :: [Double] -> [([Double], Double)] -> Double -> Double
gaussianMixture xs ms sig = sum $
    zipWith (*) (map snd ms) (map ((\m -> isoGauss m sig xs) . fst) ms)

-- Testing ---------------------------------------------------------------------

newtype MassFunction = MassFunction { getMassFunction :: [([Double], Double)] }
    deriving Show

instance Arbitrary MassFunction where
    arbitrary = do
        m  <- choose (1, 10) :: Gen Int
        ms <- replicateM 5 (replicateM m arbitrary)       :: Gen [[Double]]
        zs <- elements [ [0.1,  0.2,  0.3,  0.3,  0.1]
                       , [0.5,  0.1,  0.05, 0.05, 0.3]
                       , [0.01, 0.01, 0.01, 0.01, 0.96] ] :: Gen [Double]
        return $ MassFunction (zip ms zs)

listsOfLengthFive :: Gen [Double]
listsOfLengthFive = replicateM 5 arbitrary 

prop_isoGaussIsPositive :: [Double] -> Positive Double -> [Double] -> Bool
prop_isoGaussIsPositive mu (Positive sig) xs = isoGauss mu sig xs >= 0

prop_gaussianMixtureIsPositive :: MassFunction -> Positive Double -> Property
prop_gaussianMixtureIsPositive (MassFunction ms) (Positive sig) = 
    forAll listsOfLengthFive $ \xs -> gaussianMixture xs ms sig >= 0

