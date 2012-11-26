{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}

module Mixture (
               -- * Main functions
                 massTransformer, gaussMixer
               ) where

import Control.Pipe
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.IO
import GHC.Float

import Test.QuickCheck
import Test.QuickCheck.All

-- Utils (not exported) --------------------------------------------------------

-- | Density function for an isotropic Gaussian.  The (identity) covariance 
--   matrix is multiplied by the scalar 'sig'.
isoGauss :: [Double] -> Double -> [Double] -> Double
isoGauss mu sig xs = foldl1' (*) (zipWith density nds xs)
    where nds = map (`normalDistr` sig) mu
{-# INLINE isoGauss #-}

-- | Pretty-print a distribution to a handle.
printDist :: Handle -> [([Double], Double)] -> IO ()
printDist h = go where
    go []     = return ()
    go (a:as) = do
      hPutStrLn h $ "mode: " ++ filter (`notElem` "()") (show $ fst a)
                 ++ ", prob: " ++ show (snd a)
      go as
{-# INLINE printDist #-}

-- | Mass function over keys, where the Int values indicate the number
--   of elements in the container.
massFunction :: Fractional b => HashMap [Double] Int -> [b]
massFunction urn = 
    (map snd . HashMap.toList . HashMap.map ((/ n) . fromIntegral)) urn
  where n = fromIntegral $ HashMap.foldr (+) 0 urn
{-# INLINE massFunction #-}

-- | A mixture density of isotropic Gaussians, assembled from a list of means.
gaussianMixture :: [Double] -> [([Double], Double)] -> Double -> Double
gaussianMixture xs ms sig = sum $
    zipWith (*) (map snd ms) (map ((\m -> isoGauss m sig xs) . fst) ms)
{-# INLINE gaussianMixture #-}

-- Exported functions ----------------------------------------------------------

-- | A pipe that transforms incoming HashMaps into mass functions over their
--   contents.
massTransformer :: (Ord b, Fractional b, Monad m) 
                => Pipe (HashMap [Double] Int) [([Double], b)] m r
massTransformer = forever $ do
    urn <- await
    yield . sortBy (flip compare `on` snd) $ 
        zip (HashMap.keys urn) (massFunction urn)
 
-- | A pipe that yields isotropic Gaussian mixture densities based on an input
--   mixing distribution.  The 'sig' parameter is the variance that
--   each mixture component will have.
gaussMixer :: Double 
           -> Pipe [([Double], Double)] ([Double] -> Double) IO ()
gaussMixer sig = forever $ do
    mass <- await
    lift $ hPutStrLn stderr "Observed mixing distribution: "
    lift $ printDist stderr mass
    let logMixtureDensity xs = log $ gaussianMixture xs mass sig 
    yield logMixtureDensity
 
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

instance Arbitrary (HashMap [Double] Int) where
    arbitrary = do
        k  <- choose (1, 10)
        ks <- replicateM k (arbitrary `suchThat` ((== 2) . length)) 
        vs <- replicateM k (choose (1, 2))
        return . HashMap.fromList $ zip ks vs

listsOfLengthFive :: Gen [Double]
listsOfLengthFive = replicateM 5 arbitrary 

prop_massFunctionSumsToOne :: HashMap [Double] Int -> Bool
prop_massFunctionSumsToOne urn = (double2Float . sum . massFunction) urn == 1

prop_massFunctionElementsAreProbabilities :: HashMap [Double] Int -> Bool
prop_massFunctionElementsAreProbabilities urn = all (>= 0) m && all (<= 1) m
    where m = map double2Float $ massFunction urn

prop_isoGaussIsPositive :: NonEmptyList Double -> Positive Double 
                        -> NonEmptyList Double -> Bool
prop_isoGaussIsPositive (NonEmpty mu) (Positive sig) (NonEmpty xs) = 
    isoGauss mu sig xs >= 0

prop_gaussianMixtureIsPositive :: MassFunction -> Positive Double -> Property
prop_gaussianMixtureIsPositive (MassFunction ms) (Positive sig) = 
    forAll listsOfLengthFive $ \xs -> gaussianMixture xs ms sig >= 0

runTestSuite :: IO Bool
runTestSuite = $forAllProperties 
    (quickCheckWithResult (stdArgs {maxSuccess = 1000}))

main :: IO ()
main = void runTestSuite

