-------------------------------------------------------------------------------
-- leetcode 47 全排列 II
-- 给定一个可包含重复数字的序列，返回所有不重复的全排列。
-- 示例:
-- 输入: [1, 1, 2]
-- 输出: [[1, 1, 2], [1, 2, 1], [2, 1, 1]]
-------------------------------------------------------------------------------

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
permuteUnique xs = [ x : ys | x <- nub xs, ys <- permuteUnique $ delete x xs ]

permuteUnique' :: Eq a => [a] -> [[a]]
permuteUnique' = nub . permutations
