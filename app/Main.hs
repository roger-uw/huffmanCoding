module Main where

import qualified HuffmanCoding as H

import Control.Monad (forever)

main = forever $ do repl
                    getLine

repl = do putStrLn "Input some text: "
          input <- getLine
          putStrLn "\nHuffman Tree: \n"
          let treeOut = H.text2tree input
              codeOut = H.text2code input
          print treeOut
          putStrLn "\nHuffman Codes: \n"
          putStrLn $ H.printHuffmanCode codeOut
          putStr "Source Entropy: "
          print $ H.textEntropy input
          putStr "Average Code Length: "
          print $ H.avgLength codeOut
          putStrLn "=== END ==="