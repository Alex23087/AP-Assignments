module Main (main) where

import MultiSet
import TestMSet
import Data.List

-- main :: IO ()
-- main = 
--     let test = MS [(1,1),(2,2),(3,3),(4,4),(5,5)] in 
--     let test2 = add test 42 in do {
--     putStrLn (show (MultiSet.add (MultiSet.empty) 18));
--     putStrLn (show (MultiSet.add (MultiSet.add (MultiSet.empty) 18) 18));
--     putStrLn (show (MultiSet.add (MultiSet.add (MultiSet.empty) 18) 19));
--     putStrLn (show (occs (test) 1));
--     putStrLn (show (elems (test)));
--     putStrLn (show (subeq test test2));
--     putStrLn (show (subeq test2 test));
--     putStrLn (show (union test test2));
--     putStrLn (show (test == test));
--     putStrLn (show (test == test2));
--     putStrLn (show (foldr (+) 0 test));
--     let mapping a = [a * 2] in putStrLn (show (fmap mapping test));
--     set <- TestMSet.readMSet "app/Main.hs";
--     putStrLn (show set);
--     TestMSet.writeMSet set "app/dat.txt";
-- }

main :: IO ()
main = do{
    m1 <- TestMSet.readMSet "../aux_files/anagram.txt";
    m2 <- TestMSet.readMSet "../aux_files/anagram-s1.txt";
    m3 <- TestMSet.readMSet "../aux_files/anagram-s2.txt";
    m4 <- TestMSet.readMSet "../aux_files/margana2.txt";

    if (m1 /= m4) && ((sort (MultiSet.elems m1)) == (sort (MultiSet.elems m4)))
        then putStrLn "Statement 1 is true: Multisets m1 and m4 are not equal, but they have the same elements"
        else putStrLn "Error! Statement 1 is false";
    if m1 == (MultiSet.union m2 m3)
        then putStrLn "Statement 2 is true: Multiset m1 is equal to the union of multisets m2 and m3"
        else putStrLn "Error! Statement 2 is false";

    writeMSet m1 "anag-out.txt";
    writeMSet m4 "gana-out.txt";
}