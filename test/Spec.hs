import qualified HuffmanCoding as H

main :: IO ()
main = do let input = "asdfggfdsa"
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