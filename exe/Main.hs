module Main(main) where

import Data.Binary qualified as B
import Net
import LA

type NN = Net 400 (Size 50 ReLU (Size 50 Softmax Final)) 50

main :: IO ()
main =
  do bs <- B.decodeFile "net.bin"
     print (evalNet (bs :: NN) zeroV)

