-- 2からxまでの整数のリストを返す
getFirstList x = [2..x]

-- 先頭の整数とその倍数を削除したリストを返す
getErasedList :: [Int] -> [Int]
getErasedList list = [ y | y <- list, y `mod` head list /= 0]

{- 
 先頭の整数の2乗が最後尾の整数の値を超えるまで、
 再帰的にリスト先頭の整数を追加し続け、
 超えた時に引数となっていたリストそのものを返す
-}
getPrimeList :: [Int] -> [Int]
getPrimeList list = if head list * head list <= last list then [head list] ++ getPrimeList (getErasedList list) else list

-- 引数の値以下の素数を全て含んだリストを返す
era :: Int -> [Int]
era x = if x >= 2 then getPrimeList (getFirstList x) else []
