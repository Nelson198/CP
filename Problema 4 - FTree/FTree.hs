module FTree where

import Cp

-- (1) Datatype definition ------------------------------------------------------------------------------------------------------------------------------------------------------------------

data FTree a b = Unit b | Comp a (FTree a b) (FTree a b) deriving (Eq, Show)
type PTree = FTree Square Square
type Square = Float

-- (2) In + Out -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

inFTree :: Either b (a, (FTree a b, FTree a b)) -> FTree a b
inFTree (Left x) = Unit x
inFTree (Right (y, (ft1, ft2))) = Comp y ft1 ft2

outFTree :: FTree a1 a2 -> Either a2 (a1, (FTree a1 a2, FTree a1 a2))
outFTree (Unit x) = Left x
outFTree (Comp y ft1 ft2) = Right (y, (ft1, ft2))

-- (3) Rec + Base + Ana + Cata + Hylo -------------------------------------------------------------------------------------------------------------------------------------------------------

baseFTree :: (a1 -> b1) -> (a2 -> b2) -> (a3 -> d) -> Either a2 (a1, (a3, a3)) -> Either b2 (b1, (d, d))
baseFTree g f k = f -|- (g >< (k >< k)) 

recFTree :: (a -> d) -> Either b1 (b2, (a, a)) -> Either b1 (b2, (d, d))
recFTree k = id -|- (id >< (k >< k))

cataFTree :: (Either b1 (b2, (d, d)) -> d) -> FTree b2 b1 -> d
cataFTree c = c . (recFTree (cataFTree c)) . outFTree

anaFTree :: (a1 -> Either b (a2, (a1, a1))) -> a1 -> FTree a2 b
anaFTree a = inFTree . (recFTree (anaFTree a) ) . a

hyloFTree :: (Either b1 (b2, (c, c)) -> c) -> (a -> Either b1 (b2, (a, a))) -> a -> c
hyloFTree c a = cataFTree c . anaFTree a

-- (4) Map ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-instance Functor FTree
         where fmap f = cataFTree ( inFTree . baseFTree f id )-}

-- (5) Functions ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

generatePTree :: Int -> PTree
generatePTree i = anaFTree (either (Unit i) (Comp i, (i-1, i-1)))

--drawPTree :: PTree -> [Picture]

-- (6) Other Functions ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

depthFTree :: FTree a b -> Int
depthFTree = cataFTree (either (const 0) g)
    where g (a,(l,r)) = max l r + 1
    
{-isBalancedFTree :: FTree a b -> Bool
isBalancedFTree = isJust . cataFTree (either (const (Just 0)) g)
    where
         g (a,(l,r)) = join (liftA2 equal l r)
         equal x y = if x == y then Just (x+1) else Nothing-}