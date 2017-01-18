module HuffmanCoding
    ( HuffmanTree(..)
    , HuffmanCode(..)
    , fromTuple
    , list2tree
    , list2code
    , letterFreq
    , text2tree
    , text2code
    , avgLength
    , entropy
    , textEntropy
    , encode
    , decode
    , printHuffmanCode
    ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
-- import qualified Control.Monad as Mnd
-- import Debug.Trace

data HuffmanTree t = Node {nodeValue :: Maybe t, 
                           nodeWeight :: Double, 
                           leftChild :: HuffmanTree t, 
                           rightChild :: HuffmanTree t} 
                     | EmptyHuffmanTree

data HuffmanCode t = HuffmanCode {huffmanMsg :: t, 
                                  huffmanWeight :: Double, 
                                  huffmanCode :: String}

instance (Show t) => Show (HuffmanTree t) where
    show t = let (s, _, _) = ppTree t in collapse s
                  where collapse :: [String] -> String
                        collapse = L.foldl1' (\acc now -> acc ++ "\n" ++ now)
                        ppTree :: (Show t, Num a, Ord a) => HuffmanTree t -> ([String], a, a)
                        ppTree EmptyHuffmanTree = (["EmptyTree"], 0, 0)
                        ppTree (Node (Just m) w EmptyHuffmanTree EmptyHuffmanTree) = ([show m ++ " : " ++ show w], 0, 0)
                        ppTree (Node Nothing w l r) = (concat [addLineR, ["-- < " ++ show w], addLineL], lnL + rnL + 1, lnR + rnR + 1)
                        -- sR/sL : strings generated from right/left child
                        -- lnR/lnL : accumulative count of nodes on the left of right/left child
                        -- rnR/rnL : accumulative count of nodes on the right of right/left child
                              where (sR, lnR, rnR) = ppTree r
                                    (sL, lnL, rnL) = ppTree l
                                    addLineR = spaceR rnR sR
                                         where spaceR rn (x:xs)
                                                      | rn > 0 = ("     " ++ x) : spaceR (rn - 1) xs
                                                      | otherwise = ("   + " ++ x) : connectR lnR xs
                                                              where connectR ln [] = []
                                                                    connectR ln (x:xs)
                                                                             | ln > 0 = ("   | " ++ x) : connectR (ln - 1) xs
                                                                             | otherwise = []
                                    addLineL = connectL rnL sL
                                         where connectL rn (x:xs)
                                                        | rn > 0 = ("   | " ++ x) : connectL (rn - 1) xs
                                                        | otherwise = ("   + " ++ x) : spaceL lnL xs
                                                                where spaceL ln [] = []
                                                                      spaceL ln (x:xs)
                                                                             | ln > 0 = ("     " ++ x) : spaceL (ln - 1) xs
                                                                             | otherwise = []

instance Eq (HuffmanTree t) where
    (Node _ x _ _) == (Node _ y _ _) = x == y
    EmptyHuffmanTree == EmptyHuffmanTree = True
    _ == _ = False

instance Ord (HuffmanTree t) where
    (Node _ x _ _) <= (Node _ y _ _) = x <= y
    EmptyHuffmanTree <= _ = True
    _ <= EmptyHuffmanTree = False
    
instance (Show t) => Show (HuffmanCode t) where
    show (HuffmanCode m w s) = show m ++ " : " ++ show w ++ " -> " ++ s
    
instance Eq (HuffmanCode t) where
    (HuffmanCode _ wx _) == (HuffmanCode _ wy _) = wx == wy
    
instance Ord (HuffmanCode t) where
    (HuffmanCode _ wx _) <= (HuffmanCode _ wy _) = wx <= wy

fromTuple :: (t, Double) -> HuffmanTree t
fromTuple (x, w) = Node (Just x) w EmptyHuffmanTree EmptyHuffmanTree

buildForest :: [(t, Double)] -> [HuffmanTree t]
buildForest = L.sort . map fromTuple

buildTree :: [HuffmanTree t] -> HuffmanTree t
buildTree [] = EmptyHuffmanTree
buildTree [x] = x
buildTree (x:y:xs)
               | null xs = mergeTree y x
               | otherwise = buildTree $ L.sort (mergeTree y x : xs)
                       where mergeTree :: HuffmanTree t -> HuffmanTree t -> HuffmanTree t
                             mergeTree tx ty = Node Nothing (nodeWeight tx + nodeWeight ty) tx ty

genCode :: [HuffmanCode t] -> HuffmanTree t -> [HuffmanCode t]
genCode _ EmptyHuffmanTree = []
genCode s (Node (Just m) w EmptyHuffmanTree EmptyHuffmanTree) = HuffmanCode m w [] : s
genCode s t = map (addLetter '1') (genCode s (rightChild t)) ++ map (addLetter '0') (genCode s (leftChild t))
              where addLetter :: Char -> HuffmanCode t -> HuffmanCode t
                    addLetter c (HuffmanCode m w s) = HuffmanCode m w (c : s)

list2tree :: [(t, Double)] -> HuffmanTree t
list2tree = buildTree . buildForest

list2code :: [(t, Double)] -> [HuffmanCode t]
list2code = L.sort . genCode [] . list2tree

letterFreq :: String -> [(Char, Double)]
letterFreq xs = let n = fromIntegral (length xs)
                in M.toList $ M.map (/n) $ M.fromListWith (+) [(c, 1) | c <- xs]

text2code :: String -> [HuffmanCode Char]
text2code = list2code . letterFreq

text2tree :: String -> HuffmanTree Char
text2tree = list2tree . letterFreq

printHuffmanCode :: (Show t) => [HuffmanCode t] -> String
printHuffmanCode [] = "Not available\n"
printHuffmanCode [x] = show x ++ "[]\n"
printHuffmanCode l = L.foldl' (\acc now -> ((show now ++ "\n")++) . acc) id l []

entropy :: [(t, Double)] -> Double
entropy = negate . sum . map (((*) <*> logBase 2) . snd)

textEntropy :: String -> Double
textEntropy = entropy . letterFreq

avgLength :: [HuffmanCode t] -> Double
avgLength = sum . map ((*) . huffmanWeight <*> fromIntegral . length . huffmanCode)

-- A sortable symbol set is required to use Map for better performance
code2map :: (Ord t) => [HuffmanCode t] -> M.Map t String
code2map = M.fromList . map (\(HuffmanCode m w s) -> (m, s))

encode :: (Ord t) => [t] -> [HuffmanCode t] -> String
encode l c = let cm = code2map c 
             in  concatMap (\x -> unbox (M.lookup x cm)) l
                 where unbox :: Maybe String -> String
                       unbox (Just s) = s
                       unbox Nothing = []

decode :: String -> HuffmanTree t -> [t]
decode s tree = decode' s tree
          where decode' _ EmptyHuffmanTree = []
                decode' [] (Node Nothing _ _ _) = []
                decode' s (Node (Just m) _ EmptyHuffmanTree EmptyHuffmanTree) = m : decode' s tree
                decode' (x:xs) (Node Nothing _ l r)
                        | x == '0' = decode' xs l
                        | otherwise = decode' xs r

-- simpleTest = putStrLn $ printHuffmanCode $ list2code [(1,0.45),(2,0.30),(3,0.15),(4,0.10)]