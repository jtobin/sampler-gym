module Mixture where

import PolyaUrn
import Control.Pipe
import Control.Monad
import Control.Monad.Primitive
import Data.List
import Data.Function
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Float
import Statistics.Distribution
import Statistics.Distribution.Normal

-- NOTE try non-gaussians here


-- | Density function for an isotropic Gaussian.  The (identity) covariance 
--   matrix is multiplied by the scalar 'sig'.
isoGauss :: [Double] -> Double -> [Double] -> Double
isoGauss mu sig xs = foldl1' (*) (zipWith density nds xs)
    where nds = map (`normalDistr` sig) mu
{-# INLINE isoGauss #-}

massTransformer :: (Ord b, Fractional b, Monad m) => Pipe Urn [([Double], b)] m r
massTransformer = forever $ do
    urn <- await
    yield . sortBy (flip compare `on` snd) $ 
        zip (HashMap.keys urn) (massFunction urn)
 
mixer :: Monad m 
      => Double 
      -> Pipe [([Double], Double)] ([Double] -> Double) m r
mixer sig = forever $ do
    mass <- await
    let inter xs = foldr (+) 0 $ zipWith (*) (map snd mass) 
          (map ((\m -> isoGauss m sig xs) . fst) mass)
    yield inter
 

    

-- await urn
-- get mass function
-- 
-- for every ball in the urn, mix a standard isotropic gaussian with 
-- appropriate mixing probability.  return the mixture density.
--
-- p0*dnorm(x0, y0) + p1*dnorm(x1, y1) + ...
-- foldr (+) 0 $ zipWith (*) (massFunction urn) (dnorm . fromList $ urn)

-- await the mixture density
-- sample for however many iterations & print 

