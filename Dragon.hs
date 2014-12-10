module Dragon (dragon) where

import Command

data Symbol = X | Y | F | Plus | Minus deriving (Show)

start = [F, X]

rule :: Symbol -> [Symbol]
rule Plus  = [Plus]
rule Minus = [Minus]
rule X     = [X, Plus, Y, F]
rule Y     = [F, X, Minus, Y]
rule F     = []

dragon :: Int -> [Command]
dragon n = (iter n start) >>= toCommand

iter :: Int -> [Symbol] -> [Symbol]
iter 0 s = s
iter n s = iter (n-1) (next s)

next :: [Symbol] -> [Symbol]
next = concat . map rule

toCommand :: Symbol -> [Command]
toCommand F     = [Fwd 10]
toCommand X     = []
toCommand Y     = []
toCommand Plus  = [Rot 90]
toCommand Minus = [Rot (-90)]
