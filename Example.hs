import Nuke

main :: IO ()
main = do
    -- Create variable with mkVar
    -- Type should be the instance of Num
    let x = mkVar 'x' :: Expression Integer
    let y = mkVar 'y' :: Expression Integer

    -- You can simplify expressions
    -- Supported operations are: (+), (-), (*), (^)
    print $ (x + 1)^2
    print $ simplify $ (x + 1)^4

    print $ simplify $ x - x
    print $ simplify $ -2 * (x + 1)

    -- You can compare expressions for equality (if they are simplified)
    print $ simplify ((x + 1)^2) == simplify ((x - 1)^2 + 4 * x)

    -- You can substitute variable for another expression
    print $ simplify $ (x * y)^2 + x * y + 1
    print $ simplify $ substitute 'x' (x * y) (x^2 + x + 1)

    -- Or you can substitute for constant to evaluate it
    print $ simplify $ substitute 'x' 10 (x^2 + x + 1)

    -- You can differentiate expressions 
    print $ simplify $ diff 'x' (x^2 * y + x + y + 1)

    -- Show without simplify comes with no guarantees
    print $ x + 1 * 2

    -- Show does not guarantee order
    print $ simplify $ (x + y + mkVar 'ъ')^3

    -- This should work fine
    print $ simplify $ substitute 'x' x x 

    -- This is linear for now...
    print $ simplify $ x^1000000