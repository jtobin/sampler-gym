{-# OPTIONS_GHC -Wall -fno-warn-unused-binds #-}

import PolyaUrn    (polyaUrn)
import qualified PolyaUrn 
import Dirichlet   (dirichletMass)
import qualified Dirichlet 
import Mixture     (massTransformer, gaussMixer)
import FlatSampler (flatMcmcSampler, Options(..), MarkovChain(..))
import qualified FlatSampler as Flat

import Control.Pipe
import Control.Monad
import qualified Data.Vector as V
import Data.Word
import System.Environment
import System.Random.MWC

data GenType = Dirichlet | DirichletProcess deriving (Eq, Read)

main :: IO ()
main = do
    args <- getArgs
    let nUrnDraws    = read $ head args :: Int
        dpAlpha      = read $ args !! 1 :: Double
        gUpper       = read $ args !! 2 :: Double
        nFlatSamples = read $ args !! 3 :: Int
        nParticles   = read $ args !! 4 :: Int
        burnIn       = read $ args !! 5 :: Int
        thinEvery    = read $ args !! 6 :: Int
        prngSeed     = read $ args !! 7 :: Word32
        genType      = read $ args !! 8 :: GenType

        polyaUrnOptions = PolyaUrn.Options { PolyaUrn.grid    = (0, gUpper)
                                           , PolyaUrn.nepochs = nUrnDraws
                                           , PolyaUrn.alpha   = dpAlpha     }

        dirichletOptions = Dirichlet.Options { Dirichlet.grid  = (0, gUpper)
                                             , Dirichlet.alpha = replicate 3 dpAlpha }

        flatOptions     = Flat.Options { _size      = nParticles
                                       , _nEpochs   = nFlatSamples
                                       , _burnIn    = burnIn
                                       , _thinEvery = thinEvery
                                       , _csize     = 25           }

    g      <- initialize (V.singleton prngSeed)
    starts <- replicateM nParticles (replicateM 2 (uniformR (0, gUpper) g))

    let initState      = MarkovChain (V.fromList starts) 0
        sampleWithFlat = flatMcmcSampler flatOptions initState g

        pipe0     =     dirichletMass dirichletOptions g
                    >+> gaussMixer 1.0
                    >+> sampleWithFlat

        pipe1     =     polyaUrn polyaUrnOptions g 
                    >+> massTransformer 
                    >+> gaussMixer 1.0 
                    >+> sampleWithFlat

    case genType of 
        Dirichlet        -> runPipe pipe0
        DirichletProcess -> runPipe pipe1


