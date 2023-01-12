module MultiSet where

data MSet a = MS [(a, Int)]
    deriving (Show)

class MultiSet m where
    singleton :: a -> m a
    contains :: Eq a => m a -> a -> Bool
    add :: Eq a => m a -> a -> m a
    add mset v = addN mset v 1
    addN :: Eq a => m a -> a -> Int -> m a
    occs :: Eq a => m a -> a -> Int
    elems :: m a -> [a]
    subeq :: Eq a => m a -> m a -> Bool
    union :: m a -> m a -> m a

instance Eq a => Eq (MSet a) where
    (==) lhs rhs = True

instance MultiSet MSet where
    singleton element = MS [(element, 1)]

    contains mset element =
        let contains_aux lst el = case lst of
                ([])    ->  False
                (list)  ->  if fst(head list) == el 
                                then True
                                else contains_aux (tail list) el
        in contains_aux (get mset) element

    addN mset v n = case mset of
        MS []         ->  MS [(v, n)]
        MS (x:xs)     ->  if (fst x) == v
            then MS (((fst x), (snd x + n)):xs)
            else MS (x:(get (add (MS xs) v)))

    occs mset v = case mset of
        MS []                    -> 0
        MS (x:xs) | (fst x) == v -> snd x
                  | otherwise    -> occs (MS xs) v 

    elems mset = map fst (get mset)

    subeq mset1 mset2 = case mset1 of
        MS []                               -> True
        MS ((e,c):xs) | (occs mset2 e) > c  -> subeq (MS xs) mset2
                      | otherwise           -> False


get :: MSet a -> [(a, Int)]
get (MS list) = list

empty :: MSet a
empty = (MS [])