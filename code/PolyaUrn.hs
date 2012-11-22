{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}

module PolyaUrn where

import Control.Pipe
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Primitive
import Data.List
import Data.Maybe 
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import System.Random.MWC
import GHC.Float

import Test.QuickCheck.Gen hiding (Gen, sample)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers
import Test.QuickCheck.All
import Test.QuickCheck (quickCheckWithResult, stdArgs, maxSuccess)

-- Data/types ------------------------------------------------------------------

data Grid    = Grid {-# UNPACK #-} !Double {-# UNPACK #-} !Double 
data Options = Options { grid    :: Grid
                       , nepochs :: {-# UNPACK #-} !Int
                       , alpha   :: {-# UNPACK #-} !Double }
type Urn     = HashMap [Double] Int
type Ball    = ([Double], Int)

-- Utils -----------------------------------------------------------------------

sample :: Ord b1 => b1 -> [(b, b1)] -> Maybe b
sample p = fmap fst . find ((> p) . snd)
{-# INLINE sample #-}

-- Functions -------------------------------------------------------------------

gridBounds :: Grid -> (Double, Double)
gridBounds (Grid g0 g1) = (g0, g1)

massFunction :: Fractional b => Urn -> [b]
massFunction urn = 
    (map snd . HashMap.toList . HashMap.map ((/ n) . fromIntegral)) urn
  where n = fromIntegral $ ballsInUrn urn
{-# INLINE massFunction #-}

cumulativeProbs :: Urn -> [Double]
cumulativeProbs h = if   n > 0 
                    then   scanl1 (+) . map snd . HashMap.toList 
                         . HashMap.map ((/ n) . fromIntegral) $ h 
                    else error "cumulativeProbs: empty urn."
    where n = fromIntegral $ ballsInUrn h
{-# INLINE cumulativeProbs #-}

cDist :: Urn -> [([Double], Double)]
cDist h = uncurry zip . (HashMap.keys &&& cumulativeProbs) $ h
{-# INLINE cDist #-}

ballsInUrn :: Urn -> Int
ballsInUrn = HashMap.foldr (+) 0
{-# INLINE ballsInUrn #-}

sampleFromGrid :: PrimMonad m => Grid -> Gen (PrimState m) -> m Ball
sampleFromGrid g gen = liftM (flip (,) 1) (replicateM 2 (uniformR (g0, g1) gen))
    where (g0, g1) = gridBounds g
{-# INLINE sampleFromGrid #-}

sampleFromUrn :: PrimMonad m => Urn -> Gen (PrimState m) -> m Ball
sampleFromUrn urn g = do
    let m = fromMaybe (error "sampleFromUrn: no ball found") 
    ball <- liftM (m . (`sample` cDist urn)) (uniformR (0.0 :: Double, 1.0) g)
    let count = m (HashMap.lookup ball urn)
    return (ball, count + 1)
{-# INLINE sampleFromUrn #-}
    
-- Main ------------------------------------------------------------------------

polyaUrn :: PrimMonad m => Options -> Gen (PrimState m) -> Producer Urn m ()
polyaUrn opts g0
    | nepochs opts < 0 = error "observeProcess: iterations must be >= 0."
    | alpha   opts < 0 = error "observeProcess: alpha must be > 0."
    | otherwise        = do urn <- lift $ go n0 HashMap.empty p0 a0 g0
                            yield urn
  where 
    (n0, p0, a0) = (\x -> (nepochs x, grid x, alpha x)) opts
    go 0 u _ _ _  = return u 
    go n u p a g = do
      zc      <- uniformR (0.0, 1.0) g 
      (b, c)  <- if   zc < a / (a + fromIntegral (ballsInUrn u))
                 then sampleFromGrid p g else sampleFromUrn  u g
      go (n - 1) (HashMap.insert b c u) p a g
{-# INLINE polyaUrn #-}

-- Testing ---------------------------------------------------------------------

instance Arbitrary (HashMap [Double] Int) where
    arbitrary = do
        k  <- choose (1, 10)
        ks <- replicateM k (arbitrary `suchThat` ((== 2) . length)) 
        vs <- replicateM k (choose (1, 2))
        return . HashMap.fromList $ zip ks vs

prop_sampleFindsElements :: Ord a 
                         => OrderedList (a1, a) -> NonNegative a -> Bool
prop_sampleFindsElements (Ordered xs) (NonNegative p) = 
    if any (> p) (map snd xs) then isJust choice else isNothing choice
  where choice = sample p xs
 
prop_massFunctionSumsToOne :: Urn -> Bool
prop_massFunctionSumsToOne urn = (double2Float . sum . massFunction) urn == 1

prop_massFunctionElementsAreProbabilities :: Urn -> Bool
prop_massFunctionElementsAreProbabilities urn = all (>= 0) m && all (<= 1) m
    where m = map double2Float $ massFunction urn

prop_ballsCountedCorrectly :: Urn -> Bool
prop_ballsCountedCorrectly h = ballsInUrn h == numBalls
  where numBalls = foldr ((+) . snd) 0 (HashMap.toList h)

prop_cumulativeProbsAreProbs :: Urn -> Bool
prop_cumulativeProbsAreProbs h = 
    all (>= 0) cprobs && all (<= 1) cprobs
  where cprobs = map double2Float $ cumulativeProbs h

runTestSuite :: IO Bool
runTestSuite = $forAllProperties 
    (quickCheckWithResult (stdArgs {maxSuccess = 1000}))

