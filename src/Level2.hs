{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- | Домашка 5. Задачи для уверенности.
module Level2 where

import MetaUtils (todo)

-- 2.1. Унарные числа.
--
-- makeNat строит число из n конструкторов Suc и завершающего Zero.
-- Для отрицательного аргумента выбросьте error. foldNat применяет s ровно n раз
-- к z: foldNat (+ 1) 0 (Suc (Suc Zero)) == 2.
-- Напишите обе функции с помощью сопоставления и рекурсии.

data Nat = Zero | Suc Nat
  deriving (Eq, Show)

makeNat :: Int -> Nat
makeNat n
  | n < 0     = error "makeNat doesn't work for negative n"
  | n == 0    = Zero
  | otherwise = Suc $ makeNat (n - 1)

foldNat :: (a -> a) -> a -> Nat -> a
foldNat s z Zero    = z
foldNat s z (Suc n) = s $ foldNat s z n


-- 2.2. Другое бинарное дерево.
--
-- Empty не содержит значения. У BinNode есть два поддерева и одно значение.
-- Реализуйте mapBinTree, сохраняя форму и меняя содержимое по функции.
-- Затем напишите сигнатуру и тело bangAll: она превращает BinTree (BinTree Int)
-- в BinTree (BinTree String); после каждого числа в строке стоит "!".

data BinTree a = Empty | BinNode (BinTree a) a (BinTree a)
  deriving (Eq, Show)

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree f t = case t of
  Empty         -> Empty
  BinNode l x r -> BinNode (mapBinTree f l) (f x) (mapBinTree f r)

bangAll :: BinTree (BinTree Int) -> BinTree (BinTree String)
bangAll = mapBinTree $ mapBinTree (\n -> show n ++ "!")


-- 2.3. Лексикографическое сравнение.
--
-- lexLess xs ys истинно, если xs строго меньше ys.
-- Собственный префикс меньше списка; при первом различии решает пара элементов.
-- Равные списки не меньше друг друга. Для элементов можно использовать compare
-- и операторы сравнения; списки сравнивайте собственной рекурсией,
-- не передавайте целые списки стандартному (<) или compare.

lexLess :: Ord a => [a] -> [a] -> Bool
lexLess _ [] = False
lexLess [] _ = True
lexLess (x:xs) (y:ys)
  | x == y = lexLess xs ys
  | otherwise = x < y


-- 2.4. Цифры.
--
-- Верните цифры неотрицательного Int от старшей к младшей.
-- digits 0 == [0], digits 120 == [1, 2, 0].
-- Отрицательный аргумент — error. Используйте div/mod или quotRem,
-- без перевода числа в строку.

digits :: Int -> [Int]
digits n
  | n < 0 = error "digits don't expect negative n"
  | otherwise = go [] n
  where
    go acc n
      | n < 10    = n : acc
      | otherwise = go (n `mod` 10 : acc) (n `div` 10)


-- 2.5. Числа Фибоначчи и списки.
--
-- Бесконечный список fibs уже дан; в make repl смотрите take 10 fibs.
-- isFib проверяет принадлежность последовательности, в том числе 0 и 1.
-- Отрицательные числа не принадлежат ей.
-- countFibs считает элементы входного списка, являющиеся числами Фибоначчи;
-- повторения считаются отдельно. Например, countFibs [0, 1, 1, 4] == 3.
-- Используйте список fibs, без формулы через квадратный корень.
-- Integer выбран, чтобы последовательность не переполнялась.

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

isFib :: Integer -> Bool
isFib n = go fibs n
  where
    go (m:tail) n
      | m == n = True
      | m > n  = False
      | otherwise = go tail n
    go _ _ = False  --impossible, here for linter

countFibs :: [Integer] -> Int
countFibs = length . filter isFib


-- 2.6. Скользящее окно.
--
-- [1..6] `withWindow` 3 == [[1,2,3],[2,3,4],[3,4,5],[4,5,6]].
-- Длина каждого окна равна n; неполное последнее окно не включается.
-- Если n больше длины входа, результат []; если n <= 0, выбросьте error.
-- Функция должна позволять получить первые окна бесконечного списка.
-- Сигнатуру не меняйте.

withWindow :: [a] -> Int -> [[a]]
withWindow xs n
  | n <= 0 = error "withWindow don't expect negative n"
  | length (take n xs) < n = []
  | otherwise = take n xs : withWindow (drop 1 xs) n


-- 2.7. Сохраните проверенные опции в типе.
--
-- RawOptions пришёл с границы программы. True означает запрос справки:
-- формат и путь должны отсутствовать. False означает запуск:
-- формат обязателен, отсутствие пути означает Stdout.
-- Верните Just Options, если комбинация допустима, иначе Nothing.
-- После этого потребитель Options уже не проверяет сочетания полей.
-- Корректность содержимого FilePath в этом задании не проверяется.

data Format = Plain | JSON
  deriving (Eq, Show)

data Target = Stdout | File FilePath
  deriving (Eq, Show)

data Options = Help | Run Format Target
  deriving (Eq, Show)

data RawOptions = RawOptions Bool (Maybe Format) (Maybe FilePath)
  deriving (Eq, Show)

parseOptions :: RawOptions -> Maybe Options
parseOptions rawOption = case rawOption of
  RawOptions True Nothing Nothing -> Just Help
  RawOptions False (Just format) Nothing         -> Just $ Run format Stdout
  RawOptions False (Just format) (Just filepath) -> Just $ Run format (File filepath)
  RawOptions _ _ _    -> Nothing
