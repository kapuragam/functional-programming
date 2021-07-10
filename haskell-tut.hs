-- Comments
{-
Multiline Comments
-}

import Data.List
import System.IO

-- Haskell uses type inference. Haskell is statically typed.

-- Int -2^63 to 2^63

maxInt = maxBound :: Int
minInt = minBound :: Int

-- Integer (unbounded whole number)
-- Float

-- Double (upto 11 decimal points)

bigFloat = 3.999999999999 + 0.000000000005

-- Bool (True/False)
-- Char single unicode characters indicated with ''
-- Tuple store list made up of many different datatypes. Mostly, used to store only two values.

-- Declaring permanent value of a variable

always5 :: Int
always5 = 5

-- Math in Haskell

sumOfNums = sum [1..1000] -- [1..1000] generates a list of values from 1 to 1000

-- Infix operators

addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4    -- returns 1.25

-- Pre-fix operators

modEx = mod 5 4
modEx2 = 5 `mod` 4  -- convert prefix to infix operator using back ticks

negNumX = 5 + (-4) -- put paranthesis around -4 to give operator precedence i.e ghc will explicitly not allow 5 + -4

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)  -- since sqrt takes only float type values, we convert the int value using "fromIntegral"

-- Built-in math functions

piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.99999
roundVal = round 9.999
roundValMidPoint = round 9.5 -- will give 10
ceilingVal = ceiling 9.999
ceilingValMidpoint = ceiling 9.5 -- will give 10
floorVal = floor 9.999
floorValMidPoint = floor 9.5 -- will give 9

-- Logical operators

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

-- Lists

-- Lists in haskell are singly linked and we can add only to the front(right) of the list. They can have values of only one type inside them

primeNumbers = [3,5,7,11]

morePrimes = primeNumbers ++ [13,17,19,23,29] -- the list concatenation operator is given by ++

favNums = 2 : 3 : 7 : 2.3 : []  -- the : operator is used to combine Int, Integer, Float, Double type items into a list. [] is empty list.

multList = [[1,2,3],[123,5.0]] -- creates list of list. only numeric types in the list, can't add char types with numeric types

morePrimes2 = 2 : morePrimes

lenPrime = length morePrimes2 -- calculate length of a list

revPrime = reverse morePrimes2 -- reverse a list 

isListEmpty = null morePrimes2 -- check for empty list 

secondPrime = morePrimes2 !! 1 -- returns the value at the ith index. in this case index = 1. indexes in haskell start from 0

firstPrime = head morePrimes2 -- gives the first value of the list

lastPrime = last morePrimes2 -- gives the last value of the list

primeInit = init morePrimes2 -- gives everything but the last element of the list

first3Primes = take 3 morePrimes2 -- returns a list with the first n (=3 here) elements

removedPrimes = drop 3 morePrimes2 -- removes the first three primes

is7InList = elem 7 morePrimes2 -- check if 7 is in the list

maxPrime = maximum morePrimes2 -- find the maximum in the list

minPrime = minimum morePrimes2 -- find the minimum in the list

sumPrimes = sum morePrimes2 -- sum of all elements in the list

newList = [2,3,5]

prodNewList = product newList -- product of all elements in the list

zeroToTen = [0..10] -- generate a list from 0 to 10

evenList = [2,4..20] -- generates a list of the form 2,4,6...upto 20

letterList = ['a','c'..'z'] -- generates "acegikmoqsuwy"

infinPow10 = [10,20..] -- can create an 'infinite' list that is computed upto the point that is required when this variable is being used (lazy evaluation)

fifthElem = infinPow10 !! 5 -- generates 60 from above. infinPow10 is not computed until fifthElem is called

many2s = repeat 2 -- infinite list of repeating twos.

ten2s = take 10 many2s -- many2s is computed here (lazy evaluation) and then first 10 elements taken

many3s = replicate 10 3 -- copies the element 10 times

manyOneTwos = replicate 10 [[[1,2]]] -- generates a list of list of list of list  [[[[1,2]]],[[[1,2]]],[[[1,2]]],[[[1,2]]],[[[1,2]]],[[[1,2]]],[[[1,2]]],[[[1,2]]],[[[1,2]]],[[[1,2]]]]

manyAs = replicate 10 'a' -- copies a 10 times into a string aaaaaaaaaa

manyAstrings = replicate 10 ['a'] -- generates a list of strings ["a","a","a","a","a","a","a","a","a","a"]

cycleList = take 10 (cycle [1,2,3,4,5]) -- take 10 elements from a circular list of values  [1,2,3,4,5,1,2,3,4,5]

cycleListFloat = take 10 (cycle [1,2,3,4.2,5]) -- even if one element is float, entire list becomes float [1.0,2.0,3.0,4.2,5.0,1.0,2.0,3.0,4.2,5.0]

cycleListChars = take 17 (cycle ['a','d'..'z']) -- "adgjmpsvyadgjmpsv"

listTimes2 = [x * 2 | x <- [1..10], x * 3 <= 20] -- list comprehension. operation is to multiply by 2. input is in the middle. condition is x*3<=20 [2,4,6,8,10,12]

divisibleBy9And13 = [x | x <- [1..500], x `mod` 9 == 0, x `mod` 13 == 0] -- adding multiple conditions in list comprehension [117,234,351,468]

sortedList = reverse (sort [5,2,67,34,100,3,4,7]) -- sorts a list into descending order [100,67,34,7,5,4,3,2]

sumOfLists = zipWith (+) [1,2,3,4,5] [12,13,14,15,16] -- [13,15,17,19,21]

listBiggerThan5 = filter (>5) morePrimes -- 

evensUpto20 = takeWhile (<=20) [2,4..]

diffOfListL = foldl (-) 1 [2,3,4,5] -- gives -13 = ((((1 - 2) - 3) - 4) - 5) = -13

diffOfListR = foldr (-) 1 [2,3,4,5] -- gives -1 = 2 - (3 - (4 - (5 -1))) = 2 -(3-(4-4)) = 2-3 = -1

pow3List = [3^n | n <- [1..10]] -- gives [3,9,27,81,243,729,2187,6561,19683,59049]

multiplyTwoLists = [[x * y | x <- [1..10]] | y <- [1..10]] -- multiple inputs instead of multiple conditions. gives

{-
[[1,2,3,4,5,6,7,8,9,10],[2,4,6,8,10,12,14,16,18,20],[3,6,9,12,15,18,21,24,27,30],[4,8,12,16,20,24,28,32,36,40],
[5,10,15,20,25,30,35,40,45,50],[6,12,18,24,30,36,42,48,54,60],[7,14,21,28,35,42,49,56,63,70],[8,16,24,32,40,48,56,64,72,80],
[9,18,27,36,45,54,63,72,81,90],[10,20,30,40,50,60,70,80,90,100]]
-}

-- Tuples

-- Can have values of different types inside them

randTuple = (1, "Random Tuple")
bobSmith = ("Bob Smith", 52)

bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["name1","name2","name3","name4"]
addresses = ["addr1","addr2","addr3","addr4"]

namesAndAddresses = zip names addresses -- [("name1","addr1"),("name2","addr2"),("name3","addr3"),("name4","addr4")]

-- Creating functions, including main

addMe :: Int -> Int -> Int -- receiving an integer, and another integer and then returning an integer.
addMe x y = x + y -- Every function must return something. Functions cannot begin with a capital letter

-- addMe (take 1 [2,2..]) (take 1 [3,3..]) will not work because take returns a list and addMe takes pure Int values
-- addMe ([2,2..] !! 1) ([3,3..] !! 1) will work

-- Add tuples

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (p, q) = (x + p, y + q)

-- addTuples (1,2) (3,4) gives
-- (4,6)

-- Function as a mapping

whatAge :: Int -> String

whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge _  = "Nothing Important" -- the underscore can be any character as well

-- Recursive function

factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial (n - 1)

prodN n = product [1..n] -- this factorial much more efficientx

-- Check if input is odd

isOdd :: Int -> Bool
isOdd x = x `mod` 2 /= 0

isEven :: Int -> Bool                 -- implemented with "|", which are known as guards
isEven n
 | n `mod` 2 /= 0 = False
 | otherwise = True                   -- within guard, can only use "otherwise", which is a keyword. can't use _ or whatever
 
-- Calculate batting average 

battingAverage :: Double -> Double -> String
battingAverage hits atBats
 | avg <= 0.2 = "Very bad batting average"
 | avg <= 0.25 = "Average player"
 | avg <= 0.28 = "Good player"
 | otherwise = "Top player"
 where avg = hits / atBats
 
 

main = do
 putStrLn "What's your name"
 name <- getLine
 putStrLn ("Hello " ++ name ++ "!")