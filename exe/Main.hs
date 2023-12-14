module Main(main) where

import Control.Monad(forM_)
import GHC.TypeNats(type (*))
import Data.Binary qualified as B
import Data.Vector qualified as V
import Net
import LA
import Rng
import Act
import Train

type Len    = 12
type Input  = Len * 26
type Output = 1

type NN = Net Input (Size 50 ReLU (Size Output Sigmoid Final)) Output

main :: IO ()
main =
  do (samples, evals) <- readSamples "connected.txt" "disconnected.txt"
     bs  <- runRngM random

     putStrLn "Start training."
     yes <- runRngM (simpleTrain samples 30 1000 (bs :: NN))
     B.encodeFile "the_net.bin" yes
     putStrLn "Training done."

     putStrLn "AFTER"
     testNet bs yes evals

testNet :: IsNet i l o => Net i l o -> Net i l o -> Samples i o -> IO ()
testNet before net samples =
  forM_ samples \(ins,outs) ->
    do let actual1  = evalNet before ins
           err1     = squareErrorLoss outs actual1

       let actual2  = evalNet net     ins
           err2     = squareErrorLoss outs actual2

       putStrLn $ "expected: " ++ show outs ++
               ", actual_before: " ++ show actual1 ++
               ", actual_after: " ++ show  actual2 ++
               ", err_diff: " ++ show (err1 - err2)


readInputs :: FilePath -> IO (V.Vector (Vector Input))
readInputs file = V.fromList . map stringToVec . lines <$> readFile file

readSamples ::
  FilePath -> FilePath -> IO ( V.Vector (Vector Input, Vector Output)
                             , V.Vector (Vector Input, Vector Output)
                             )
readSamples file0 file1 =
  do ins0 <- readInputs file0
     let (train0,eval0) =  V.splitAt 100 (((,zero) <$> ins0))
     ins1 <- readInputs file1
     let (train1,eval1) = V.splitAt 100 (((,vOneAt 0) <$> ins1))
     pure (train0 <> train1, eval0 <> eval1)

charToVec :: Char -> Vector 26
charToVec c = vOneAt p
  where
  p | n < start || n > end = error ("not a small letter " ++ show c)
    | otherwise            = n - start
  n     = fromEnum c
  start = fromEnum 'a'
  end   = fromEnum 'z'

stringToVec :: String -> Vector Input
stringToVec = vConcat . map charToVec . take 12
