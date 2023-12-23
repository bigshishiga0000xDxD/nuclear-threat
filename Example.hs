import Nuke

main :: IO ()
main = do
    let x = mkVar 'x' :: Expression Integer

    print $ (x + 1)^2
    print $ simplify $ (x + 1)^4

    print $ simplify $ x - x
    print $ simplify $ -2 * (x + 1)
    print $ simplify ((x + 1)^2) == simplify ((x - 1)^2 + 4 * x)

    let y = mkVar 'y' :: Expression Integer

    print $ simplify $ (x * y)^2 + x * y + 1
    print $ simplify $ substitute 'x' (x * y) (x^2 + x + 1)
    print $ simplify $ substitute 'x' 10 (x^2 + x + 1)

    print $ simplify $ diff 'x' (x^2 * y + x + y + 1)