import PolyaUrn
import Mixture
import Sampler
import Control.Pipe
import Control.Monad
import Control.Monad.Trans
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
    g    <- create
    let n0 = read $ head args     :: Int
        a0 = read $ args !! 1     :: Double
        gUpper = read $ args !! 2 :: Double
        nFlatSamples = read $ args !! 3 :: Int
        nParticles   = read $ args !! 4 :: Int

        opts    = Options { nepochs = n0 
                          , alpha   = a0
                          , grid    = Grid 0 gUpper } 

    let pipe0 = runUrn opts g >+> massTransformer >+> printer
        pipe1 = runUrn opts g >+> massTransformer >+> gaussMixer 1.0 >+> sampler nFlatSamples nParticles opts g

    runPipe pipe1

