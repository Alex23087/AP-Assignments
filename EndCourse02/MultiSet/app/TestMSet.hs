module TestMSet where

import MultiSet
import System.IO
import Data.List
import Data.Char

readMSet :: String -> IO (MSet String)
readMSet filename = do {
    input <- readFile filename;
    return (((foldr (\x -> \y -> MultiSet.add y x) MultiSet.empty) . (\x -> map sort x) . words . (\x -> map toLower x)) input);
}

writeMSet :: (MSet String) -> String -> IO () -- <elem> - <multiplicity>
writeMSet mset filename = do {
    writeFile filename (foldr (\x -> \y -> y ++ "<" ++ (fst x) ++ "> - <" ++ (show (snd x)) ++ ">\n") "" (MultiSet.get mset))
}

main :: IO ()
main = do{
    m1 <- TestMSet.readMSet "../../aux_files/anagram.txt";
    m2 <- TestMSet.readMSet "../../aux_files/anagram-s1.txt";
    m3 <- TestMSet.readMSet "../../aux_files/anagram-s2.txt";
    m4 <- TestMSet.readMSet "../../aux_files/margana2.txt";

    if (m1 /= m4) && ((sort (MultiSet.elems m1)) == (sort (MultiSet.elems m4)))
        then putStrLn "Statement 1 is true: Multisets m1 and m4 are not equal, but they have the same elements"
        else putStrLn "Error! Statement 1 is false";
    if m1 == (MultiSet.union m2 m3)
        then putStrLn "Statement 2 is true: Multiset m1 is equal to the union of multisets m2 and m3"
        else putStrLn "Error! Statement 2 is false";

    writeMSet m1 "anag-out.txt";
    writeMSet m4 "gana-out.txt";
}