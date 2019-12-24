module PermuteUnique
    ( permuteUnique
    , permuteUnique'
    )
where



import           Data.List                      ( nub
                                                , delete
                                                , permutations
                                                )


permuteUnique :: Eq a => [a] -> [[a]]
permuteUnique [] = [[]]
permuteUnique xs = [ x : ys | x <- nub xs, ys <- permuteUnique (delete x xs) ]

permuteUnique' :: Eq a => [a] -> [[a]]
permuteUnique' = nub . permutations
