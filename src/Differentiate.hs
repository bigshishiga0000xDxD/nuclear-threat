module Differentiate where

import Expression
import Operation

diff :: Num numType => Char -> Expression numType -> Expression numType
diff var (Leaf (Left varNode)) =
    if var == varNode then 1 else 0
diff var (Leaf (Right num)) = 0
diff var (Node OperationSum args) =
    Node OperationSum $ map (diff var) args
diff var (Node OperationMul args) =
    Node OperationSum [Node OperationMul (left ++ [diff var mid] ++ right) |
                       i <- [0..length args - 1], let (left, mid : right) = splitAt i args]
