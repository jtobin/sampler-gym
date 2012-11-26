import PolyaUrn    (polyaUrn, Options(..), Grid(..))
import Mixture     (massTransformer, gaussMixer)
import FlatSampler (flatMcmcSampler, Options(..), MarkovChain(..))
import qualified FlatSampler as Flat
import Control.Pipe
import Control.Monad
import Control.Monad.Trans
import qualified Data.Vector as V
import Data.Word
import System.Environment
import System.Random.MWC

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

        polyaUrnOptions = PolyaUrn.Options { grid    = Grid 0 gUpper
                                           , nepochs = nUrnDraws
                                           , alpha   = dpAlpha       }

        flatOptions     = Flat.Options { _size      = nParticles
                                       , _nEpochs   = nFlatSamples
                                       , _burnIn    = burnIn
                                       , _thinEvery = thinEvery
                                       , _csize     = 25           }

    g      <- initialize (V.singleton prngSeed)
    starts <- replicateM nParticles (replicateM 2 (uniformR (0, gUpper) g))

    let initState = MarkovChain (V.fromList starts) 0
        pipe0     =     polyaUrn polyaUrnOptions g 
                    >+> massTransformer 
                    >+> gaussMixer 1.0 
                    >+> flatMcmcSampler flatOptions initState g

    runPipe pipe0

