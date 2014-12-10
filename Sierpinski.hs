module Sierpinski (sierp) where

import Command

data Symbol = A | B | Plus | Minus deriving (Show)

start = A

rule :: Symbol -> [Symbol]
rule Plus  = [Plus]
rule Minus = [Minus]
rule A     = [B, Minus, A, Minus, B]
rule B     = [A, Plus,  B, Plus,  A]

sierp :: Int -> [Command]
sierp n = map toCommand (iter n [A])

iter :: Int -> [Symbol] -> [Symbol]
iter 0 s = s
iter n s = iter (n-1) (next s)

next :: [Symbol] -> [Symbol]
next = concat . map rule

toCommand :: Symbol -> Command
toCommand A     = Fwd 10
toCommand B     = Fwd 10
toCommand Plus  = Rot 60
toCommand Minus = Rot (-60)
