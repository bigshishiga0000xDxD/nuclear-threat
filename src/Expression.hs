module Expression where

import Data.List
import Data.Char
import Data.Hashable

import Operation

data Expression numType = Leaf (Either Char numType) | Node Operation [Expression numType]

isNum :: Num numType => Expression numType -> Bool
isNum (Leaf (Right x)) = True
isNum _                = False


getNum :: Num numType => Expression numType -> numType
getNum (Leaf (Right x)) = x


isVar :: Num numType => Expression numType -> Bool
isVar (Leaf (Left c)) = True
isVar _               = False


getVar :: Num numType => Expression numType -> Char
getVar (Leaf (Left c)) = c


getArgs :: Num numType => Expression numType -> [Expression numType]
getArgs (Node op args) = args


instance (Num numType) => Num (Expression numType) where
    x + y = Node OperationSum [x, y]
    x * y = Node OperationMul [x, y]
    negate x = Node OperationMul [fromInteger (-1), x]

    fromInteger x = Leaf $ Right $ fromInteger x

    abs = undefined
    signum = undefined

instance (Show numType, Num numType, Hashable numType) => Show (Expression numType) where
    show (Leaf (Left var)) = [var]
    show (Leaf (Right num)) = show num
    show (Node OperationMul (coef' : exprs')) = case coef of
            Just x  -> if length all' == 1
                       then x' ++ rendered
                       else x' ++ "(" ++ rendered ++ ")"
                       where x' = if x == Leaf (Right (-1))
                                  then "-"
                                  else show x

            Nothing -> rendered
        where (coef, exprs) = if isNum coef'
                then (Just coef', exprs')
                else (Nothing, coef' : exprs')
              variables = sort . map getVar . filter isVar $ exprs
              powers = map (\list -> (head list, length list)) $ group variables
              others = filter (not . isVar) exprs
              powers' = map (\(b, e) -> if e == 1
                                      then [b]
                                      else [b] ++ "^" ++ show e) powers
              showExpr node@(Node OperationSum args) = '(' : show node ++ ")"
              showExpr node = show node
              others' = map showExpr others
              all' = powers' ++ others'
              rendered = head all' ++ concatMap (show OperationMul ++) (tail all')
    show (Node op exprs) =
         head exprs' ++ concatMap (show op ++) (tail exprs')
         where exprs' = map show exprs


instance (Num numType, Hashable numType) => Eq (Expression numType) where
    x == y = hashExpr x == hashExpr y


substitute :: Num numType => Char -> Expression numType -> Expression numType -> Expression numType
substitute var val expr = case expr of
    Node op exprs         -> Node op $ map (substitute var val) exprs
    Leaf (Left nodeVar)   -> if var == nodeVar then val else Leaf $ Left nodeVar
    Leaf (Right num)      -> Leaf $ Right num


hashExpr :: (Num numType, Hashable numType) => Expression numType -> Int
hashExpr (Leaf (Right x)) = hashWithSalt 1 x
hashExpr (Leaf (Left c)) = hashWithSalt 2 c
hashExpr (Node op args) = hashWithSalt 3 (show op) * sum (map hashExpr args)