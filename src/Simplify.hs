{-# LANGUAGE TupleSections #-}

module Simplify where

import Data.List
import Data.Hashable

import Expression
import Operation

simplifyLevel :: (Num numType, Hashable numType) => Expression numType -> Expression numType
simplifyLevel leaf@(Leaf x) = leaf
simplifyLevel node@(Node op args) = case (length processed, op) of
        (0, OperationSum) -> 0
        (0, OperationMul) -> 1
        (1, OperationSum) -> head processed
        (1, OperationMul) -> head processed
        _                 -> Node op processed
    where reduced = if length processed == 1 then head processed else Node op processed
          processed = sortVars . evalNums . concatMap mapArg $ args
          mapArg node@(Node op' args') = if op == op' then args' else [node]
          mapArg leaf = [leaf]
          evalNums args =
            let nums = filter isNum args in
            let exprs = filter (not . isNum) args in
            let accum = mapOperation op $ map getNum nums in
                case (op, accum) of
                    (OperationSum, 0) -> exprs
                    (OperationSum, _) -> exprs ++ [Leaf $ Right accum]
                    (OperationMul, 1) -> exprs
                    (OperationMul, 0) -> [Leaf $ Right 0]
                    (OperationMul, _) -> Leaf (Right accum) : exprs
          sortVars args =
            let vars = filter isVar args in
            let others = filter (not . isVar) args in
                others ++ (map (Leaf . Left) . sort . map getVar $ vars)


simplifyNode :: (Num numType, Hashable numType) => Expression numType -> Expression numType
simplifyNode leaf@(Leaf x) = leaf
simplifyNode node = case simplified of 
        Node OperationSum _    -> simplifySum simplified
        Node OperationMul _    -> simplifyMul simplified
        _                      -> simplified
    where simplified = simplifyLevel node

simplifySum :: (Num numType, Hashable numType) => Expression numType -> Expression numType
simplifySum leaf@(Leaf x) = leaf
simplifySum node@(Node OperationSum args) = simplifyLevel $ Node OperationSum simplified
    where isMul (Node OperationMul args) = True
          isMul                        _ = False
          muls = filter isMul args
          others = filter (not . isMul) args
          coefs' = map ((\(x : xs) -> if isNum x
                                        then (Node OperationMul xs, getNum x)
                                        else (Node OperationMul (x : xs), 1)) . getArgs) muls ++
                  map (, 1) others
          unique = nub $ map fst coefs'
          coefs = [sum . map snd . filter ((== expr) . fst) $ coefs' | expr <- unique]
          simplified = zipWith (\coef expr -> simplifyLevel $ Leaf (Right coef) * expr) coefs unique

simplifyMul :: (Num numType, Hashable numType) => Expression numType -> Expression numType
simplifyMul leaf@(Leaf x) = leaf
simplifyMul node@(Node OperationMul args) = simplifySum $ Node OperationSum unfolded
    where isSum (Node OperationSum args) = True
          isSum _                        = False
          sums = filter isSum args
          others = filter (not . isSum) args
          brackets = map getArgs sums ++ map return others
          unfolded = map (simplifyLevel . Node OperationMul) $ sequence brackets

simplify :: (Num numType, Hashable numType) => Expression numType -> Expression numType
simplify expr = case expr of
    Node op exprs -> simplifyNode $ Node op $ map simplify exprs
    leaf -> leaf