module Part1.Tasks where

import Util(notImplementedYet)

factorial 0 = 1
factorial n = n * factorial (n - 1)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x
    | abs x <= 2 * pi = foldl (+) 0 $ map (\i -> (-1) ** (i - 1) * x ** (2 * i - 1) / (factorial (2 * i - 1))) $ [1..20]
    | x > 2 * pi = mySin $ x - 2 * pi
    | otherwise = mySin $ x + 2 * pi

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x
    | abs x <= 2 * pi = foldl (+) 0 $ map (\i -> (-1) ** (i - 1) * x ** (2 * i - 2) / (factorial (2 * i - 2))) $ [1..20]
    | x > 2 * pi = myCos $ x - 2 * pi
    | otherwise = myCos $ x + 2 * pi

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b
    | a == 0 = abs b
    | b == 0 = abs a
    | otherwise = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y 
    | m `elem` [1, 3, 5, 7, 8, 10, 12] = 1 <= d && d <= 31
    | m `elem` [4, 6, 9, 11] = 1 <= d && d <= 30
    | m == 2 = (y `rem` 400 == 0 && 1 <= d && d <= 29) || (y `rem` 4 == 0 && y `rem` 100 /= 0 && 1 <= d && d <= 29) || (1 <= d && d <= 28)
    | otherwise = False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow n 0 = 1
myPow n i = n * myPow n (i - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n = not $ foldl (||) False $ map (\i -> n `mod` i == 0) $ [2..(n - 1)]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet

xs :: [Point2D] -> [Double]
xs points = map (\(x, y) -> x) points

ys :: [Point2D] -> [Double]
ys points = map (\(x, y) -> y) points

shapeArea [] = 0
shapeArea points = abs $ 0.5 * (foldl (+) 0 $ map (\(x, y) -> x * y) $ zip (xs points) ((tail $ ys points) ++ [head $ ys points])) - 0.5 * (foldl (+) 0 $ map (\(x, y) -> x * y) $ zip (ys points) ((tail $ xs points) ++ [head $ xs points]))

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник

ordered :: Double -> Double -> Double -> (Double, Double, Double)
ordered a b c
    | a >= b && a >= c = if (b >= c) then (a, b, c) else (a, c, b)
    | b >= a && b >= c = if (a >= c) then (b, a, c) else (b, c, a)
    | otherwise = if (a >= b) then (c, a, b) else (c, b, a)

triangleKindOrdered :: (Double, Double, Double) -> Integer
triangleKindOrdered (a, b, c)
    | a >= b + c = -1
    | a * a > b * b + c * c = 0
    | a * a == b * b + c * c = 2
    | otherwise = 1

triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = triangleKindOrdered $ ordered a b c
