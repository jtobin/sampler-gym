import PolyaUrn
import Mixture
import Sampler
import Control.Pipe
import Control.Monad
import Control.Monad.Trans
import Data.Vector (singleton)
import Data.Word
import System.Environment
import System.Random.MWC

printer :: Show b => Consumer b IO ()
printer = forever $ do
    x <- await
    lift $ print x

opts = Options { nepochs = 100
               , alpha   = 1.0
               , grid    = Grid 0 10 }

main = do
    args <- getArgs
    let nUrnDraws    = read $ head args :: Int
        dpAlpha      = read $ args !! 1 :: Double
        gUpper       = read $ args !! 2 :: Double
        nFlatSamples = read $ args !! 3 :: Int
        nParticles   = read $ args !! 4 :: Int
        prngSeed     = read $ args !! 4 :: Word32

        opts    = Options { nepochs = nUrnDraws 
                          , alpha   = dpAlpha
                          , grid    = Grid 0 gUpper } 

    g <- initialize (singleton prngSeed)

    let pipe0 =     polyaUrn opts g 
                >+> massTransformer 
                >+> gaussMixer 1.0 
                >+> flatMcmcSampler nFlatSamples nParticles opts g

    runPipe pipe0

