module Permute
    ( permute
    , permute'
    )
where

import           Data.List                      ( delete
                                                , permutations
                                                )

-- 给定一个没有重复数字的序列，返回其所有可能的全排列。
-- 示例:
-- 输入: [1,2,3]
-- 输出:
-- [
--   [1,2,3],
--   [1,3,2],
--   [2,1,3],
--   [2,3,1],
--   [3,1,2],
--   [3,2,1]
-- ]


permute :: [Int] -> [[Int]]
permute [] = []
permute xs = [ x : ys | x <- xs, ys <- permute (delete x xs) ]

permute' :: [a] -> [[a]]
permute' = permutations
