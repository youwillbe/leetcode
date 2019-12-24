module TwoSum
    ( twoSum
    )
where

-- 给定一个整数数组 nums 和一个目标值 target，请你在该数组中找出和为目标值的那 两个 整数，并返回他们的数组下标。
-- 你可以假设每种输入只会对应一个答案。但是，你不能重复利用这个数组中同样的元素。
-- 示例:
-- 给定 nums = [2, 7, 11, 15], target = 9
-- 因为 nums[0] + nums[1] = 2 + 7 = 9
-- 所以返回 [0, 1]

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum list target = safeHead
    [ (y1, y2) | (x1, y1) <- r, (x2, y2) <- drop (y1 + 1) r, x1 + x2 == target ]
    where r = zip list [0 ..]
