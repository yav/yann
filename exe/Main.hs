module Main(main) where

import Control.Monad(foldM, mapAndUnzipM)
import GHC.TypeNats(type (*))
import Data.Binary qualified as B
import Data.Vector qualified as V
import Data.Char(toLower)
-- import System.Console.ANSI
import Net
import LA
import Rng
import Train
import Sample

type Len    = 12
type Input  = Len * 26
type Output = 1
type Sampl  = Sample Input Output

type NN = Net Input (Size 30 ReLU (Size Output Sigmoid Final)) Output

epochs = 100000
batch = 10

main :: IO ()
main =
  do dataSets <- readSamples "connected.txt" "disconnected.txt"
     (trainData,testData, randomNet, trainedNet) <-
        runRngM
           do (train,test) <- prepSets dataSets 
              rNet <- random
              tNet <- simpleTrain train batch epochs (rNet :: NN)
              pure (train,test,rNet,tNet)

     B.encodeFile "the_net.bin" trainedNet
     putStrLn "TRAINING"
     testNet randomNet trainedNet trainData
     putStrLn "PREDICTION"
     testNet randomNet trainedNet testData

loadNet :: IO (String -> Bool)
loadNet =
  do n <- B.decodeFile "the_net.bin"
     pure \x -> valFromVec (evalNet (n :: NN) (stringToVec x))

testNet :: IsNet i l 1 => Net i l 1 -> Net i l 1 -> Samples i 1 -> IO ()
testNet randomNet trainedNet samples =
  do (tot,correct,better,worse) <-
      (\xs a f -> foldM f a xs) samples (0::Int, 0::Int, 0::Int, 0::Int)
        \(tot,correct,better,worse)  s ->
        do let ins = sampleInput s
               outs = sampleOutput s
               outV = valFromVec outs :: Bool
 
           let actualR  = evalNet randomNet ins
               outR     = valFromVec actualR
 
           let actualT  = evalNet trainedNet ins
               outT     = valFromVec actualT
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
                , incIf (outV == outT) correct
                , incIf (outV /= outR && outV == outT) better
                , incIf (outV == outR && outV /= outT) worse
                )
     let perc x = show x ++ "(" ++
                  show (round (fromIntegral x * 100 /
                                (fromIntegral tot :: Float)) :: Int) ++ "%)"
     putStrLn ("Samples: " ++ show tot)
     putStrLn ("Correct: " ++ perc correct)
     putStrLn ("Improved: " ++ perc better)
     putStrLn ("Worsened: " ++ perc worse)

readInputs :: FilePath -> Bool -> IO (V.Vector Sampl)
readInputs file out =
  V.fromList . map (`makeSample` out) <$> ioTxt 
  where
  ioTxt = lines <$> readFile file
  


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
     let smallest = minimum (map V.length trainSets)
     pure (V.concat (map (V.take smallest) trainSets), V.concat testSets)

--------------------------------------------------------------------------------

charToVec :: Char -> Vector 26
charToVec c
  | n < start || n > end = error ("not a small letter " ++ show c)
  | otherwise            = vOneAt v -- vFromList (replicate v 1)
  where
  n     = fromEnum c
  v     = n - start
  start = fromEnum 'a'
  end   = fromEnum 'z'

stringToVec :: String -> Vector Input
stringToVec = vConcat . map charToVec . take 12
