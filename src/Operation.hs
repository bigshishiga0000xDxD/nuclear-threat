module Operation where

data Operation =
    OperationSum |
    OperationMul
    deriving Eq


instance Show Operation where
    show OperationSum = " + "
    show OperationMul = " * "


mapOperation :: Num numType => Operation -> [numType] -> numType
mapOperation OperationSum = sum
mapOperation OperationMul = product
