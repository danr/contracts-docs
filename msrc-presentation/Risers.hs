module Risers where

import Contracts
import Prelude hiding ((<=))

data Nat = S Nat | Z

(<=) :: Nat -> Nat -> Bool
Z   <= _   = True
_   <= Z   = False
S x <= S y = x <= y

le_cf = (<=) ::: CF --> CF --> CF

risers :: [Nat] -> [[Nat]]
risers [] = []
risers [x] = [[x]]
risers (x:y:xs) = case risers (y:xs) of
    s:ss | x <= y    -> (x:s):ss
         | otherwise -> [x]:(s:ss)
    [] -> error "internal error"

risers_contract = risers ::: CF :&: Pred (not . null) --> CF :&: Pred (not . null)
  `Using` le_cf

risers_rec :: [Nat] -> [[Nat]]
risers_rec [] = []
risers_rec [x] = [[x]]
risers_rec (x:y:xs) = case risers_rec_call (y:xs) of
    s:ss | x <= y    -> (x:s):ss
         | otherwise -> [x]:(s:ss)
    [] -> error "internal error"

risers_rec_call xs = risers_rec_call xs

risers_rec_contract =
    (risers_rec_call ::: CF :&: Pred (not . null) --> CF :&: Pred (not . null) :=>
     risers_rec ::: CF :&: Pred (not . null) --> CF :&: Pred (not . null)
    )
  `Using` le_cf
