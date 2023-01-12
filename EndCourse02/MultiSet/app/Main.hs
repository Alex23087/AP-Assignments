module Main (main) where

import MultiSet

main :: IO ()
main = let test = MS [(1,1),(2,2),(3,3),(4,4),(5,5)] in do {
    putStrLn (show (MultiSet.add (MultiSet.empty) 18));
    putStrLn (show (MultiSet.add (MultiSet.add (MultiSet.empty) 18) 18));
    putStrLn (show (MultiSet.add (MultiSet.add (MultiSet.empty) 18) 19));
    putStrLn (show (occs (test) 1));
    putStrLn (show (elems (test)));
}