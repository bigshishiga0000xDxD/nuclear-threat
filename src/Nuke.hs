module Nuke (
        Expression,
        mkVar,
        substitute,
        simplify,
        diff
) where

import Expression
import Simplify
import Differentiate

import Data.Char


mkVar :: Num numType => Char -> Expression numType
mkVar c | isAlpha c = Leaf $ Left c
        | otherwise = error ("Cannot make variable " ++ (c : ": should be a latin letter"))
