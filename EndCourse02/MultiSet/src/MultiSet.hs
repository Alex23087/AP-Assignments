module MultiSet where

data MSet a = MS [(a, Int)]
    deriving (Show)

class MultiSet m where
    singleton :: a -> m a
    contains :: Eq a => m a -> a -> Bool
    add :: Eq a => m a -> a -> m a
    add mset v = addN mset v 1
    addN :: Eq a => m a -> a -> Int -> m a
    remove :: Eq a => m a -> a -> m a
    occs :: Eq a => m a -> a -> Int
    elems :: m a -> [a]
    subeq :: Eq a => m a -> m a -> Bool
    union :: Eq a => m a -> m a -> m a
    equals :: Eq a => m a -> m a -> Bool

instance Eq a => Eq (MSet a) where
    (==) lhs rhs = equals lhs rhs

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
        MS []                           -> MS [(v, n)]
        MS ((e,c):xs)   | e == v        -> MS ((e, c + n):xs)
                        | otherwise     -> MS ((e,c) : get (addN (MS xs) v n))

    remove mset element = case mset of
        MS []                           -> mset
        MS ((e,c):xs)   | element == e  -> MS xs
                        | otherwise     -> MS ((e,c) : (get (remove (MS xs) element)))

    occs mset v = case mset of
        MS []                    -> 0
        MS (x:xs) | (fst x) == v -> snd x
                  | otherwise    -> occs (MS xs) v 

    elems mset = map fst (get mset)

    subeq mset1 mset2 = case mset1 of
        MS []                                   -> True
        MS ((e,c):xs) | (occs mset2 e) >= c     -> subeq (MS xs) mset2
                      | otherwise               -> False

    union mset1 mset2 = case mset1 of
        MS []           -> mset2
        MS ((e,c):xs)   -> union (MS xs) (addN mset2 e c)

    equals mset1 mset2 = case mset1 of
        MS []           | (get mset2) == []     -> True
                        | otherwise             -> False
        MS ((e,c):xs)   | (occs mset2 e) == c   -> equals (MS xs) (remove mset2 e)
                        | otherwise             -> False


instance Foldable MSet where
    foldr f a m = case m of
        MS []           -> a
        MS ((e,c):xs)   -> f e (foldr f a (MS xs))


get :: MSet a -> [(a, Int)]
get (MS list) = list

empty :: MSet a
empty = (MS [])

mapMSet :: (a -> b) -> MSet a -> MSet b
mapMSet f mset = case mset of
    MS [] -> MS []
    MS ((e,c):xs) -> MS (((f e),c) : (get (mapMSet f (MS xs))))

instance Functor MSet where
    fmap = mapMSet