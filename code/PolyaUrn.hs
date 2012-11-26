{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}

module PolyaUrn  ( 
                 -- * Data structures
                   Options(..), Urn
                 -- * Main functions
                 , polyaUrn
                 ) where

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

-- | Options by which to run the Polya Urn.
data Options = Options { -- | The Grid to run the Urn scheme over.
                         grid    :: (Double, Double)
                         -- | The number of epochs to observe the Dirichlet
                         --   process.
                       , nepochs :: {-# UNPACK #-} !Int
                         -- | The concentration parameter of the Dirichlet 
                         --   process.  Higher numbers yield more clusters.
                       , alpha   :: {-# UNPACK #-} !Double }

type Urn     = HashMap [Double] Int
type Ball    = ([Double], Int)

-- Utils (not exported) --------------------------------------------------------

-- | A sampled value from a cumulative distribution.
sample :: Ord b1 => b1 -> [(b, b1)] -> Maybe b
sample p = fmap fst . find ((> p) . snd)
{-# INLINE sample #-}

-- | The cumulative probabilities of an urn's contents.
cumulativeProbs :: Urn -> [Double]
cumulativeProbs h = if   n > 0 
                    then   scanl1 (+) . map snd . HashMap.toList 
                         . HashMap.map ((/ n) . fromIntegral) $ h 
                    else error "cumulativeProbs: empty urn."
    where n = fromIntegral $ ballsInUrn h
{-# INLINE cumulativeProbs #-}

-- | The cumulative mass function of an urn.
cDist :: Urn -> [([Double], Double)]
cDist h = uncurry zip . (HashMap.keys &&& cumulativeProbs) $ h
{-# INLINE cDist #-}

-- | The number of balls in the urn.
ballsInUrn :: Urn -> Int
ballsInUrn = HashMap.foldr (+) 0
{-# INLINE ballsInUrn #-}

-- | Sample a new ball from the grid.
sampleFromGrid :: PrimMonad m => (Double, Double) -> Gen (PrimState m) -> m Ball
sampleFromGrid (g0, g1) gen = liftM (flip (,) 1) (replicateM 2 (uniformR (g0, g1) gen))
{-# INLINE sampleFromGrid #-}

-- | Take an existing ball out of an urn.
sampleFromUrn :: PrimMonad m => Urn -> Gen (PrimState m) -> m Ball
sampleFromUrn urn g = do
    let m = fromMaybe (error "sampleFromUrn: no ball found") 
    ball <- liftM (m . (`sample` cDist urn)) (uniformR (0.0 :: Double, 1.0) g)
    let count = m (HashMap.lookup ball urn)
    return (ball, count + 1)
{-# INLINE sampleFromUrn #-}

-- Exported functions ----------------------------------------------------------

-- | A Producer that yields an observed Dirichlet process having a 2D Gaussian
--   base measure, via a Polya Urn scheme.
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

main :: IO ()
main = void runTestSuite

