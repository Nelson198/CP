module Blockchain where

import Cp
import List

-- (1) Datatype definition ------------------------------------------------------------------------------------------------------------------------------------------------------------------

type MagicNo = String
type Time = Int
type Entity = String
type Value = Int

type Block = (MagicNo, (Time, Transactions))
type Transaction = (Entity, (Value, Entity))
type Transactions = [Transaction]
type Ledger = [(Entity, Value)]

data Blockchain = Bc {bc :: Block} | Bcs {bcs :: (Block, Blockchain)} deriving Show

-- (2) In + Out -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

inBlockchain :: Either Block (Block, Blockchain) -> Blockchain
inBlockchain = either Bc Bcs

outBlockchain :: Blockchain -> Either Block (Block, Blockchain)
outBlockchain (Bc block) = Left block
outBlockchain (Bcs (block, blockchain)) = Right (block, blockchain) 

-- (3) Rec + Base + Ana + Cata + Hylo -------------------------------------------------------------------------------------------------------------------------------------------------------
baseBlockchain :: (a -> b) -> (c -> d) -> Either a (a, c) -> Either b (b, d)
baseBlockchain f k = f -|- (f >< k)

recBlockchain :: (c -> d) -> Either b1 (b2, c) -> Either b1 (b2, d)
recBlockchain k = id -|- (id >< k)

cataBlockchain :: (Either Block (Block, d) -> d) -> Blockchain -> d
cataBlockchain c = c . (recBlockchain (cataBlockchain c)) . outBlockchain 

anaBlockchain :: (c -> Either Block (Block, c)) -> c -> Blockchain
anaBlockchain a = inBlockchain . (recBlockchain (anaBlockchain a)) . a

hyloBlockchain :: (Either Block (Block, c1) -> c1) -> (c2 -> Either Block (Block, c2)) -> c2 -> c1
hyloBlockchain c a = cataBlockchain c . anaBlockchain a

-- (4) Functions ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

allTransactions :: Blockchain -> Transactions
allTransactions = cataBlockchain (either (p2 . p2) (conc . ((p2 . p2) >< id)))

---------------------------------------------------------------------------------------------------------------------------------

updateLedger ::  (Transaction, Ledger) -> Ledger
updateLedger ((e1, (v, e2)), []) = []
updateLedger ((e1, (v, e2)), ((entidade, valor) : t)) | e1 == entidade = (entidade, valor-v) : updateLedger(((e1, (v, e2)), t))
                                                      | e2 == entidade = (entidade, valor+v) : updateLedger(((e1, (v, e2)), t))
                                                      | otherwise = (entidade, valor) : updateLedger(((e1, (v, e2)), t))

ledger :: Blockchain -> Ledger
ledger = cataList (either nil updateLedger) . allTransactions

---------------------------------------------------------------------------------------------------------------------------------

check :: [MagicNo] -> Bool
check [m] = True
check (x : xs) | existe x xs == True = False
               | otherwise = check xs

existe :: Eq a => a -> [a] -> Bool
existe _ [] = False
existe x (h:t) = if x == h then True else (existe x t)

isValidMagicNr :: Blockchain -> Bool
isValidMagicNr = check . cataBlockchain (either (singl . p1) (cons . (p1 >< id)))

-- (5) Examples -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

e1 = "Ent1"
e2 = "Ent2"

mg1 = "MagicNo1"
mg2 = "MagicNo2"
mg3 = "MagicNo3"

tm1 = 1
tm2 = 2
tm3 = 3

t1 = (e1, (100, e2))
t2 = (e2, (50, e1))
t3 = (e1, (150, e2))

bl1, bl2, bl3 :: Block
bl1 = (mg1, (tm1, []))
bl2 = (mg2, (tm2, [t1]))
bl3 = (mg3, (tm3, [t2]))
bl4 = (mg3, (tm3, [t3]))

bs1, bs2 :: Blockchain
bs1 = Bcs (bl1, Bcs (bl2, Bc bl3))
bs2 = Bcs (bl3, Bcs (bl2, Bc bl2))
bs3 = Bc bl2
bs4 = Bcs (bl1, Bc bl3)
bs5 = Bc bl1
bs6 = Bcs (bl2, Bcs (bl4, Bc bl1))

-- (6) Other Functions ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

reverseChain :: Blockchain -> Blockchain
reverseChain = cataBlockchain (either Bc snocChain)

snocChain :: (Block, Blockchain) -> Blockchain
snocChain (b, Bc b') = Bcs (b', Bc b)
snocChain (b, Bcs (b', bs)) = Bcs (b', snocChain (b, bs))

concChain :: (Blockchain, Blockchain) -> Blockchain
concChain (b1, (Bc b)) = snocChain (b, b1)
concChain (b1, (Bcs (b, bs))) = concChain ((snocChain (b, b1)), bs)

lenChain = cataBlockchain (either (const 1) (succ . p2))