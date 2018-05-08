module QTree where

import Cp

-- (1) Datatype definition ------------------------------------------------------------------------------------------------------------------------------------------------------------------

data QTree a = Cell a Int Int | Block (QTree a) (QTree a) (QTree a) (QTree a) deriving (Eq, Show)

-- (2) In + Out -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

inQTree :: Either (a, (Int, Int)) (QTree a, (QTree a, (QTree a, QTree a))) -> QTree a
inQTree (Left (a, (x, y))) = Cell a x y
inQTree (Right (q1, (q2, (q3, q4)))) = Block q1 q2 q3 q4

outQTree :: QTree a -> Either (a, (Int, Int)) (QTree a, (QTree a, (QTree a, QTree a)))
outQTree (Cell a x y) = Left (a, (x, y))
outQTree (Block q1 q2 q3 q4) = Right (q1, (q2, (q3, q4)))

-- (3) Rec + Base + Ana + Cata + Hylo -------------------------------------------------------------------------------------------------------------------------------------------------------

recQTree :: (a -> d1) -> Either (b, d2) (a, (a, (a, a))) -> Either (b, d2) (d1, (d1, (d1, d1)))
recQTree k = (id >< id) -|- (k >< (k >< (k >< k)))

baseQTree :: (a1 -> b) -> (a2 -> d1) -> Either (a1, d2) (a2, (a2, (a2, a2))) -> Either (b, d2) (d1, (d1, (d1, d1)))
baseQTree f k = (f >< id) -|- (k >< (k >< (k >< k)))

cataQTree :: (Either (b, (Int, Int)) (d, (d, (d, d))) -> d) -> QTree b -> d
cataQTree c = c . (recQTree (cataQTree c)) . outQTree

anaQTree :: (a1 -> Either (a2, (Int, Int)) (a1, (a1, (a1, a1)))) -> a1 -> QTree a2
anaQTree a = inQTree . (recQTree (anaQTree a)) . a

hyloQTree :: (Either (b, (Int, Int)) (c, (c, (c, c))) -> c) -> (a -> Either (b, (Int, Int)) (a, (a, (a, a)))) -> a -> c
hyloQTree c a = cataQTree c . anaQTree a

-- (4) Map ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance Functor QTree
         where fmap f = cataQTree ( inQTree . baseQTree f id )

-- (5) Functions ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rotateQTree :: QTree a -> QTree a
