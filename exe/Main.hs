module Main(main) where

import Control.Monad(forM, foldM, mapAndUnzipM)
import GHC.TypeNats(type (*))
import Data.Binary qualified as B
import Data.Vector qualified as V
import System.Console.ANSI
import Net
import LA
import Rng
import Act
import Train
import Sample

type Len    = 12
type Input  = Len * 26
type Output = 1
type Sampl  = Sample Input Output

type NN = Net Input (Size 20 ReLU (Size Output Sigmoid Final)) Output

main :: IO ()
main =
  do dataSets <- readSamples "connected.txt" "disconnected.txt"
     (trainData,testData, randomNet, trainedNet) <-
        runRngM
           do (train,test) <- prepSets dataSets 
              rNet <- random
              tNet <- simpleTrain train 16 10000 (rNet :: NN)
              pure (train,test,rNet,tNet)

     B.encodeFile "the_net.bin" trainedNet
     putStrLn "TRAINING"
     testNet randomNet trainedNet trainData
     putStrLn "PREDICTION"
     testNet randomNet trainedNet testData

testNet :: IsNet i l 1 => Net i l 1 -> Net i l 1 -> Samples i 1 -> IO ()
testNet before net samples =
  do (tot,correct,better,worse) <-
      (\xs a f -> foldM f a xs) samples (0, 0, 0, 0)
        \(tot,correct,better,worse)  s ->
        do let ins = sampleInput s
               outs = sampleOutput s
               outV = valFromVec outs :: Bool
 
           let actual1  = evalNet before ins
               outV1    = valFromVec actual1
 
           let actual2  = evalNet net ins
               outV2    = valFromVec actual2
 {-
           let showB b = if b then "T" else "F"
           let printV x o v =
                 do let c = if v == outV then Green else Red
                    setSGR [SetColor Foreground Dull c]
                    putStr ("[" ++ x ++ "] " ++ showB v ++ " (" ++ show o ++ ")")
                    setSGR [Reset]
           putStr (sampleLabel s ++ ": ")
           printV "before" actual1 outV1
           putStr ", "
           printV "after" actual2 outV2
           putStrLn (if outV == outV1 && outV /= outV2 then " WORSE" else "")
          -}
           let incIf p x = if p then x + 1 else x
           pure ( tot + 1
                , incIf (outV == outV2) correct
                , incIf (outV /= outV1 && outV == outV2) better
                , incIf (outV == outV1 && outV /= outV2) worse
                )
     let perc x = show (round (fromIntegral x * 100 / (fromIntegral tot :: Float)) :: Int) ++ "%"
     putStrLn ("Samples: " ++ show tot)
     putStrLn ("Correct: " ++ perc correct)
     putStrLn ("Improved: " ++ perc better)
     putStrLn ("Worsened: " ++ perc worse)

readInputs :: FilePath -> Bool -> IO (V.Vector Sampl)
readInputs file out =
  V.fromList . map (`makeSample` out) . lines <$> readFile file

makeSample :: String -> Bool -> Sampl
makeSample ins out =
  Sample
    { sampleLabel = ins ++ ": " ++ show out
    , sampleInput = stringToVec ins
    , sampleOutput = valToVec out
    }

readSamples ::
  FilePath -> FilePath -> IO [V.Vector Sampl]
readSamples file0 file1 =
  do ins0 <- readInputs file0 False
     ins1 <- readInputs file1 True
     pure [ins0, ins1]

prepSet :: V.Vector Sampl -> RngM (V.Vector Sampl, V.Vector Sampl)
prepSet xs =
  do ys <- shuffle xs
     pure (V.splitAt (V.length xs `div` 2) ys)

prepSets :: [V.Vector Sampl] -> RngM (V.Vector Sampl, V.Vector Sampl)
prepSets sets =
  do (trainSets,testSets) <- mapAndUnzipM prepSet sets
     pure (V.concat trainSets, V.concat testSets)

--------------------------------------------------------------------------------

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
