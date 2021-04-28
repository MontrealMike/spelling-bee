-- ======================================================================================
-- New York Times Spelling Bee
-- ======================================================================================
{-  Implementation of NYT Spelling Bee Puzzle

    Michael Bleau
    2020-11-17

    Done:
        embed single use functions into main functions using let - in or where
        use classes for dictionnary tuples and other things
        respond to commands
        manage memory footprint by controlling imports - 2020-11-28 <- limiting imports has no memory impact
    ToDo:
        use Curses for IO
-}

module Main where

-- ======================================================================================
-- Imports
-- ======================================================================================

import qualified Control.Monad
import qualified Data.Array as A (Array, array, (!))
import qualified Data.Char as C (toLower)
import qualified Data.List as L (isSubsequenceOf, map, nub, sort)
import qualified Data.Map as Map
  ( Map,
    empty,
    findWithDefault,
    foldlWithKey,
    fromList,
    size,
    toList,
  )
import System.IO (hFlush, stdout)
import Text.Printf as Printf (printf)

-- ======================================================================================
-- Custom data types
-- ======================================================================================
type Dictionnary = [(String, String)] -- (UniqueLetters, Word)

type Solution = Map.Map String (Int, Bool) -- (Word (WordScore, PangramFlag))

type UserSolution = ([String], Int, Int) -- ([Word] TotalScore, PangramCount)

-- ======================================================================================
-- Main function
-- ======================================================================================
main :: IO ()
main = do
  testTrellis
  printWelcome
  rawDict <- getDict
  let dict = (wordTuples . legalWords) rawDict
  putDictSummary dict
  puzzle <- getPuzzleLetters
  putPuzzleLetters puzzle
  let solution = solvePuzzle puzzle dict
  userSolve puzzle solution

{- print welcome -}
printWelcome :: IO ()
printWelcome = do
  putStrLn ""
  putStrLn "Spelling bee puzzle - V0.5"
  putStrLn "=========================="
  putStrLn ""

{- get user input -}
prompt :: String -> IO String
prompt p = do
  putStr p
  hFlush stdout
  getLine

-- ======================================================================================
-- Dictionnary functions
-- ======================================================================================
{- Generate a list of dictWords -}
getDict :: IO [String]
getDict =
  fmap words (readFile "/usr/share/dict/american-english")

{- filter out illegal dictWords -}
legalWords :: [String] -> [String]
legalWords x =
  let capitalized x
        | null x = False
        | otherwise = elem (head x) ['A' .. 'Z']
      longEnough x = length x > 3
      containsQuote x
        | null x = False
        | otherwise = elem '\'' x
   in [lw | lw <- x, not (capitalized lw), longEnough lw, not (containsQuote lw)]

{- generate a list of word tuples -}
wordTuples :: [String] -> [(String, String)]
wordTuples x =
  let uniqueLetters x = L.sort (L.nub x)
   in [(uniqueLetters wt, wt) | wt <- x]

{- sort pair tuples by first value -}
sortPairsByFst :: Ord a => [(a, b)] -> [(a, b)]
sortPairsByFst [] = [] -- empty  list
sortPairsByFst [x] = [x] -- single item in list
sortPairsByFst l =
  let left = take (length l `div` 2) l
      right = drop (length l `div` 2) l
   in mergePairsByFst (sortPairsByFst left) (sortPairsByFst right)
  where
    {- merge pair tuples by first value
       this could be improved by replacing `fst` with a parameter
    -}
    mergePairsByFst :: Ord a => [(a, b)] -> [(a, b)] -> [(a, b)]
    mergePairsByFst [] l2 = l2 -- 1st list empty return second
    mergePairsByFst l1 [] = l1 -- 2nd list empty return first
    mergePairsByFst (x : xs) (y : ys) =
      if fst x > fst y
        then x : mergePairsByFst xs (y : ys)
        else y : mergePairsByFst (x : xs) ys

{- output dictionnary summary -}
putDictSummary :: Dictionnary -> IO ()
putDictSummary d =
  putStrLn ("Dictionnary has " ++ show (length d) ++ " words")

-- ======================================================================================
-- Create puzzle fuctions
-- ======================================================================================
{- get a list of letters
   duplicate letters are eliminated
   capital letters are converted to lower case
   the first letter is compulsory
   the remaing ones are optional -}
getPuzzleLetters :: IO String
getPuzzleLetters = do
  ltrs <- prompt "Puzzle letters (compulsory letter first): "
  let validLetters = L.nub (L.map C.toLower ltrs)
  return validLetters

{- output puzzle letters -}
putPuzzleLetters :: String -> IO ()
putPuzzleLetters [] = do
  putStrLn "Puzzle has no letters"
  putStrLn ""
putPuzzleLetters (x : xs) = do
  putStrLn ("Puzzle has " ++ show (length (x : xs)) ++ " letters: " ++ [x] ++ "-" ++ xs)
  putStrLn ""

-- ======================================================================================
-- Program solve functions
-- ======================================================================================
{- solve puzzle
   get all words that a) contain the compulsory letter and b) only contain legal letters
   l  letters
   wt word tuples
   solution is stored in a Data Map keyed on the word and containing a tuple of score and pangram status
-}
solvePuzzle :: String -> [(String, String)] -> Solution
solvePuzzle [] _ = Map.empty
solvePuzzle (l : ls) wt =
  Map.fromList ([scoreWord (snd x) | x <- wt, l `elem` fst x, L.isSubsequenceOf (fst x) (L.sort (l : ls))])
  where
    {-  score a word
        assumes the provided word is valid (a part of the solution)
        the word gets a point for each letter above three
        and a 7 point bonus if it is a pangram
    -}
    scoreWord :: String -> (String, (Int, Bool))
    scoreWord w =
      let isPangram = length (L.nub w) == 7 -- pangrams have 7 unique letters
       in (w, (length w - 3 + if isPangram then 7 else 0, isPangram))

{-  puzzle stats
      s  a solution
      returns a 3 Integer tuple with the number of words, the total score and the the number of pangrams
-}
calcSolutionStats :: Solution -> (Int, Int, Int)
calcSolutionStats s =
  let totScore (x, _) score = score + x
      pangramCount (_, x) pangrams = pangrams + if x then 1 else 0
   in ( Map.size s,
        foldr totScore 0 s,
        foldr pangramCount 0 s
      )

{- output solution -}
putSolution :: Solution -> IO ()
putSolution s = do
  putStrLn "----------- Solution Words -------------------------------------------------"
  print s
  putStrLn "----------------------------------------------------------------------------"

-- ======================================================================================
-- User solve functions
-- ======================================================================================
userSolve :: String -> Solution -> IO ()
userSolve puzzle solution = do
  let userSolve1 :: String -> Solution -> UserSolution -> String -> IO ()
      -- empty puzzle
      userSolve1 [] _ _ _ = return ()
      --empty solution
      userSolve1 puzzle solution userSolution [] = do
        printProgress puzzle solution userSolution
        userInput <- getUserInput
        userSolve1 puzzle solution userSolution userInput
      -- help
      userSolve1 puzzle solution userSolution "?" = do
        printHelp
        userSolve1 puzzle solution userSolution ""
      -- command (with colon ':' prefix)
      userSolve1 puzzle solution userSolution (':' : command) = do
        keepGoing <- processCommand command puzzle solution userSolution
        Control.Monad.when keepGoing $ userSolve1 puzzle solution userSolution []
      -- candidate word
      userSolve1 puzzle solution (userWords, cumScore, pangramCount) userWord =
        let (wordScore, pangramFlag) = Map.findWithDefault (0, False) userWord solution
         in if wordScore == 0
              then do
                putStrLn " word not found"
                userSolve1 puzzle solution (userWords, cumScore, pangramCount) []
              else do
                putStr (" Good, " ++ show wordScore ++ " point(s)")
                putStrLn (if pangramFlag then ", Pangram!" else "")
                userSolve1
                  puzzle
                  solution
                  ( userWord : userWords,
                    wordScore + cumScore,
                    pangramCount + if pangramFlag then 1 else 0
                  )
                  []
  -- initial call with empty user solution and no user input
  userSolve1 puzzle solution ([], 0, 0) []

getUserInput :: IO String
getUserInput =
  prompt "Enter word or command (? for help): "

printProgress :: String -> Solution -> UserSolution -> IO ()
printProgress p s (userWords, userScore, userPangrams) = do
  let (sWords, sScore, sPangrams) = calcSolutionStats s
  putStrLn "----------- Solution Stats -------------------------------------------------"
  printf "%12s %12s %12s\n" p "Possible" "So far"
  printf "%12s %12d %12d\n" "Word count:" sWords (length userWords)
  printf "%12s %12d %12d\n" "Score:" sScore userScore
  printf "%12s %12d %12d\n" "Pangrams:" sPangrams userPangrams
  putStrLn ""
  printUserWords userWords
  putStrLn "----------------------------------------------------------------------------"

printUserWords :: [String] -> IO ()
printUserWords [] = return ()
printUserWords w = do
  let sw = L.sort w
      printUserWords1 :: [String] -> IO ()
      printUserWords1 [] = return ()
      printUserWords1 (word : words) = do
        putStr word
        if not (null words)
          then do
            putStr ", "
            printUserWords1 words
          else putStrLn ""
  printUserWords1 sw

processCommand :: String -> String -> Solution -> UserSolution -> IO Bool
processCommand command puzzle solution userSolution
  | null command =
    return True
  | command == "r" = do
    printSolution solution userSolution
    return True
  | command == "q" = do
    putStrLn "Good-bye"
    return False
  | otherwise = do
    putStrLn "Unknown command"
    return True

{-  print solution -}
printSolution :: Solution -> UserSolution -> IO ()
printSolution s us = do
  putStrLn "Solution -------------------------------------------------"
  let printEntry :: String -> String -> (Int, Bool) -> String
      printEntry iString w scores =
        iString ++ w ++ " - " ++ show (fst scores) ++ (if snd scores then "P" else "") ++ "\n"
  putStrLn (Map.foldlWithKey printEntry "" s)

{- print help -}
printHelp :: IO ()
printHelp = do
  putStrLn "\nSpelling Bee Help --------------------------------------"
  putStrLn "At the prompt you may:"
  putStrLn "   - enter a word (entries are not case sensitive)"
  putStrLn "   - ask for help by entering '?'"
  putStrLn "   - enter a command.  All commands start with ':'"
  putStrLn "   - commands are:"
  -- putStrLn "        :p show my progress"
  putStrLn "        :q quit"
  putStrLn "        :r reveal solution"
  putStrLn "------------------------------------------------------------\n"

-- ======================================================================================
-- unused code
-- ======================================================================================

{-  generate a list of unique letter strings from a list of words
        coverts a list of dictionnary tuples into a
        list of tuples whose first element is a unique letter combination
        and whose second is a list of all the words that can be formed by that combination
    x   a list of tuples [(sorted letters used in word, word)]
        the list must be sorted by the first value in each tuple (sorted letters)
-}
uniqueLetterDict :: Ord a => [(a, String)] -> [(a, [String])]
uniqueLetterDict x =
  let uniqueLetterDict1 [] x = x -- end of recursion no more words
      uniqueLetterDict1 ((a, b) : xs) [] =
        -- start of recursion - no items processed yet
        uniqueLetterDict1 xs [(a, [b])]
      uniqueLetterDict1 ((a, b) : xs) ((c, d) : ys) =
        --  heart of recursion
        if a == c
          then uniqueLetterDict1 xs ((a, b : d) : ys)
          else uniqueLetterDict1 xs ((a, [b]) : (c, d) : ys)
   in uniqueLetterDict1 x []

-- ======================================================================================
-- Combinatorial Trellis and lexical ordering
-- ======================================================================================
{- factorial ------------------------------------------------------------------------- -}
fact :: Integer -> Integer
fact 0 = 1
fact 1 = 1
fact n = n * fact (n -1)

{- n choose k - calculate number of permutations and combinations -}
nChoosek :: Integer -> Integer -> Integer
nChoosek n k
  | n == 0 || k == 0 = 0
  | n < k = 0
  | n == k = 1
  | otherwise = fact n `div` (fact (n - k) * fact k)

{- -----------------------------------------------------------------------------------------
    functions to implement Combinatorial Coding and Lexicographic ordering

    as described in:
        Combinatorial Coding and Lexicographic Ordering
        Peter Kabal
        Department of Electrical & Computer Engineering
        McGill University
        Montreal, Canada
-------------------------------------------------------------------------------------------- -}
testTrellis :: IO ()
testTrellis = do
  putStrLn "Testing trellis functions for n = 26, k = 7, i = [0, 1, 2, 3, 20] "
  let p0 = [0, 1, 2, 3, 4, 5, 6]
  let p1 = [0, 1, 2, 3, 4, 5, 7]
  let p2 = [x + 1 | x <- p0]
  -- let p2 = [0, 1, 2, 3, 4, 5, 6, 7]

  let t = trellis 26 7
  let i0 = getPCIndex t p0
  let i1 = getPCIndex t p1
  let i2 = getPCIndex t p2

  print ("Seq 1 = 0 " ++ show i0 ++ ": " ++ show (getPCChoices t 26 5 i0))
  print ("Seq 2 = 1 " ++ show i1 ++ ": " ++ show (getPCChoices t 26 5 i1))
  print ("Seq 3 = 1 " ++ show i2 ++ ": " ++ show (getPCChoices t 26 5 i2))

{- build a trellis used in functions that will convert an index value into a list of allowed
   combinations and back
   n is the number of items to choose from
   k is the number of possible choices -}
trellis :: Integer -> Integer -> A.Array (Integer, Integer) Integer
trellis n k =
  A.array
    ((0, 0), (k - 1, n - 1))
    [((i, j), nChoosek j (i + 1)) | i <- [0 .. (k -1)], j <- [0 .. (n -1)]]

{- get the index of a given sequence of choices.
   trellis - a preconstructed sufficiently sized trellis
   choices - array of choices eg [0, 1, 3] means choosing the 0th, 1st and 3rd item
             choices are 0-based (0 is first choice)
             it is up to the caller to ensure that
                the array contains unique values and
                the array values are in ascending order
                the number of array elements < the number of array rows
                the last (and therefore largest) array entry  < the number of array columns
  the index value returned will range from 0 to the (maximum number of combinations - 1)
-}
getPCIndex :: A.Array (Integer, Integer) Integer -> [Integer] -> Integer
getPCIndex trellis choices =
  let getPCIndex1 trellis x [] = 0
      getPCIndex1 trellis x (y : ys) =
        trellis A.! (x, y) + getPCIndex1 trellis (x + 1) ys
   in getPCIndex1 trellis 0 choices

{- get the choices based on an index
  trellis - a preconstructed trellis with at least as many rows (n) and columns (k)
            as specified by the next two parameters
  n       - the number items to choose from
  k       - the number of choices
  index   - the index of the specific list of choices
            it must have a value ranging from 0 to 1 less than the max number of choices

  returns: a list of integers
-}
getPCChoices :: A.Array (Integer, Integer) Integer -> Integer -> Integer -> Integer -> [Integer]
getPCChoices trellis n k index
  | k == 0 = []
  | otherwise =
    if index < combiIndex
      then L.sort (getPCChoices trellis (n - 1) k index) -- try one column to the left
      else L.sort (n : getPCChoices trellis (n - 1) (k - 1) (index - combiIndex))
  where
    combiIndex = trellis A.! (k - 1, n - 1)

{- -}
