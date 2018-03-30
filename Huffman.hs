import Data.List
import System.IO


data Pair a = Pair {cnt::Int, el::a} 
instance Eq (Pair a) where
       (Pair c1 _) == (Pair c2 _) = c1 == c2
    
instance Ord (Pair a )  where
      (Pair c1 _) `compare` (Pair c2 _) = c1 `compare` c2

instance Show a => Show (Pair a) where
     show (Pair c el) = "(" ++ show c ++ ","++show el ++ ")"
       
      
data HFtree a = Empty | Node (Pair a) (HFtree a) (HFtree a)
              deriving (Show, Eq, Ord)
              
              
--takes an element and makes a leaf 
--whose root is the element
makeLeaf :: (Int, a) -> HFtree a
makeLeaf (cnt,a) = Node (Pair cnt a)  Empty Empty

--get two nodes and makes a new one
--whose root is the sum of the counters 
--in the two nodes' roots and a null value  
combineNodes :: HFtree a-> HFtree a -> a -> HFtree a
combineNodes f@ (Node (Pair ca a) _ _ ) s@ (Node (Pair cb b) _ _ ) nv=
              Node (Pair (ca+cb) nv) f s
                
cntOccur::  a ->(a->a->Bool) -> [a] -> Int
cntOccur _ _ [] = 0
cntOccur e pr (x:xs) 
              | pr e x = 1 + (cntOccur e pr xs) 
              | otherwise = cntOccur e pr xs

uniques ::  [a] -> (a->a->Bool) -> [a]
uniques [] _ = []
uniques (x:xs) pr = x: uniques (filter (\e -> not (pr e x)) xs) pr

--makes a list of tuples (occurrInStr,symbol)             
getOccurrences:: [a] -> (a->a->Bool)-> [(Int,a)]
getOccurrences [] _ = []
getOccurrences str pr =  zip ( map (\c-> cntOccur c pr str) uniq) uniq
           where uniq = uniques str pr
 
--takes a list of values , predicate for comparison,
--a null value to add in the tree nodes which are not leaves
--and makes the Huffman Tree (nv is for more readability of the tree)
buildHuffmanTree :: Ord a => [a] -> (a->a->Bool) -> a-> HFtree a
buildHuffmanTree  []  _  _ = Empty
buildHuffmanTree str pr nv = huffTreeHelp minHeap nv     
       where minHeap = map makeLeaf (sort $ getOccurrences str pr)


huffTreeHelp :: [HFtree a] -> a-> HFtree a
huffTreeHelp [] _= Empty
huffTreeHelp (x:y:[]) nv = combineNodes x y nv 
huffTreeHelp minHeap nv = huffTreeHelp newHeap nv
        where fmin = minimum minHeap;  
              nHeap = delete fmin minHeap; 
              smin = minimum nHeap;
              newNode = combineNodes fmin smin nv;
              newHeap = newNode : (delete smin nHeap);
             
--makes a list of tuples (symbol, code)
--according to a given Huffman tree
encode ::   HFtree a -> [(a,String)]  
encode hft = encodeHelp hft "" 
             
encodeHelp :: HFtree a -> String -> [(a,String)] 
encodeHelp Empty _ = []
encodeHelp (Node (Pair num c) Empty Empty) code = [(c,code)]
encodeHelp (Node _ l r) code = 
               (encodeHelp l (code++"0")) ++ (encodeHelp r (code++"1"))
           
--gets the corresponidng code for a given symbol
getCode:: a ->(a->a->Bool) ->[(a,String)] -> String
getCode _ _[] = ""
getCode c pr (x:xs) 
               | pr c (fst x) = snd x
               | otherwise = getCode c pr xs               

--takes a list of values, predicate for comparison and a
--table with the codes and returns the compressed bytestring
compress:: [a] ->(a->a->Bool) -> [(a,String)] -> String    
compress [] _ _ = ""
compress (c:cs) pr table = (getCode c pr table) ++ (compress cs pr table) 


doEncoding ::Ord a => [a] -> (a->a->Bool)-> a -> (HFtree a, String)
doEncoding s  pr nv = (hft, compress s pr $ encode hft)
         where hft = buildHuffmanTree s pr nv


decode :: HFtree a -> HFtree a-> String -> [a]
decode (Node (Pair num c) Empty Empty) hft s 
                 |s == "" = [c] 
                 |otherwise = [c] ++ (decode hft hft s)
decode (Node _ l r) hft (x:xs) 
                 | x == '0' = decode l hft xs
                 | x == '1' = decode r hft xs
    
doDecoding :: HFtree a-> String -> [a]
doDecoding Empty _ = []
doDecoding hft comprStr = decode hft hft comprStr


string1 = "akkfaehki"   
string2 = "abracadabra"  
          
hft1 = buildHuffmanTree string1 (==) '#'
cms1 = snd $ doEncoding string1 (==) '#'

hft2 = buildHuffmanTree string2 (==) '#'
cms2 = snd $ doEncoding string2 (==) '#'

os = "ontology ogoss philosophers socks logo"
hftO = buildHuffmanTree os (==) '#'
cmsO = snd $ doEncoding os (==) '#'
--takes filename of the file to compress and
--writes the compressed string to Res file
--and the corresponding Huffman tree to HTF file
compressFile :: String -> IO ()
compressFile name = do
    {
        rFile <- openFile name ReadMode;
        contents <- hGetContents rFile ;    
        wStrFile <- openFile (nameOnly ++ "Res.txt") WriteMode;
        wHFtFile <- openFile (nameOnly ++ "HFT.txt") WriteMode;
        hPutStrLn wStrFile $ snd $ doEncoding contents (==) '#';
        hPutStrLn wHFtFile $ show $ fst $ doEncoding contents (==) '#';
        hClose wStrFile;
        hClose wHFtFile;
        hClose rFile ;
    }
    where nameOnly = take (length name - 4) name    ;

{-
decompressFile :: String -> IO ()
decompressFile strFile treeFile = do
    {
        rStrFile <- openFile strFile ReadMode;
        string <- hGetContents rStrFile ; 
        rTreeFile <- openFile treeFile ReadMode;
        tree <- hGetContents rTreeFile ;
        wFile <- openFile ("londonOrigRes.txt") WriteMode;
        hPutStrLn wFile $ doDecoding tree string
        hClose wFile;
        hClose rStrFile ;
        hClose rTreeFile
    }
-}  
