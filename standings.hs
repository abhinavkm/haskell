import Data.List
import Data.Char
import Data.Ord
import Data.Function


----------------------------------------------------------------
-- CS 352 - Project 1 (starter file)
--
-- Abhinav Mulagada
--
----------------------------------------------------------------

----------------------------------------------------------------
-- function exec - reads the contents of the file "input.txt",
--   and creates an index of the words; it writes the result to
--   standard output
--
-- **** YOU SHOULD NOT HAVE TO CHANGE THIS FUNCTION ****
--
-- calling sequence:
--   exec
--
-- example:
--   (See example in the function 'createIndex'.)
--
-- status:
--   Incomplete.  Presently, it just echos the contents of the
--   input file to standard output, because the 'createIndex' function is
--   dummied up.
--
----------------------------------------------------------------
exec:: IO()
exec =
  do input <- readFile "input.txt"
     putStr (createStandings input)

----------------------------------------------------------------
-- function exec2 - reads the contents of the file "input.txt",
--   and creates an index of the words; it writes the result to
--   the file "output.txt"
--
-- **** YOU SHOULD NOT HAVE TO CHANGE THIS FUNCTION ****
--
-- calling sequence:
--   exec2
--
-- example:
--   (See example in the function 'createIndex'.)
--
-- status:
--   Incomplete.  Presently, it just echos the contents of the
--   input file to "output.txt", because the 'createIndex' function is
--   dummied up.
----------------------------------------------------------------
exec2:: IO()
exec2 =
  do input <- readFile "input.txt"
     writeFile "output.txt" (createStandings input)

----------------------------------------------------------------
-- function createStandings - treats its String argument as the contents of
-- a document that contains game results; produces list of standings,
-- ordered by win-loss record, of all the teams that have competed.
--
-- **** THIS IS THE FUNCTION YOU NEED TO CHANGE ****
--
-- calling sequence:
--   createStandings str
--
-- example:
--   If the contents of the String 'str' is
--     Broncos 1 Pilots 2
--     Zags 2 Pilots 4
--     Zags 0 Broncos 0
--     Pilots 2 Broncos  3
--     Pilots 4 Zags 1
--     Broncos 1 Zags 0
--   Then the result should be:
--          Team       W       L       T      GF      GA
--       -------   -----   -----   -----   -----   -----
--        Pilots       3       1       0      12       7
--       Broncos       2       1       1       5       4
--          Zags       0       3       1       3       9
--   In other words, each team is listed in order of wins (W) minus losses
--   (L), with the tiebreaker based on goals for (GA) minus goals against
--   (GA). (If still tied, use alphabetical order of team name.)
--
-- status:
--   Incomplete.  Presently, it just echos returns the contents of the
--   'str'.
--
----------------------------------------------------------------

convertStringtoInt:: [String]->[(String, (Integer, Integer))]
convertStringtoInt [a,b,c,d] = [(a, (read b, read d)), (c, (read d, read b))]

-- Group by first element
firstEl (a,_) (b,_) = a == b
groupByFirstEl = groupBy firstEl

-- Group by team SITE:
byTeam = foldr fld []
    where
      fld (a,b) [] = [(a,[b])]
      fld (a,b) c@((x,y):xs)  | x == a = (x,b:y):xs
                           | otherwise = (a,[b]):c

-- Create five tuple format
fiveTuple:: (Integer, Integer) -> (Integer, Integer, Integer, Integer, Integer)
fiveTuple (a,b) =
   if a == b then (0,0,1,a,b)
   else if (a > b) then (1,0,0,a,b)
   else (0,1,0,a,b)

convertToFive (a,b) = (a, map fiveTuple b)

-- Sum all five tuples
toTuple (a,b) = (a, map sum . transpose $ map toList b)
toList(a,b,c,d,e) = [a,b,c,d,e]

-- Sort table by goal difference
sortScore ((a, [b, c, d, e, f])) ((g, [h, i, j, k, l]))
  | (b-c) <= (h-i) = GT
  | (b-c) >= (h-i) = LT
sortByRecord = sortBy sortScore

-- Print in correct format
unList:: (String, [Integer]) -> [String]
unList (a,[b,c,d,e,f]) = [a,show b, show c, show d, show e, show f]

padRight num str = take num (str ++ repeat ' ')
padLeft num str = reverse (padRight num (reverse str))

-- Header for table
header a = [["Team", "W", "L", "T", "GF", "GA"]] ++
  [["-----", "-----", "-----", "-----", "-----","-----"]] ++ a

-- Format table with correct padding based on team name length
format:: [[String]] -> [[String]]
format a = map (padding (findLength ((transpose a) !! 0)  ) ) a

padding:: Int -> [String] -> [String]
padding num a = map (padLeft num) a

-- Enhancement to handle team names of arbitrary length
findLength:: [String] -> Int
findLength a = length (maximumBy (comparing length) a)

createStandings:: String -> String
createStandings = unlines . (map unwords) . format . header . (map unList) .
  sortByRecord . (map toTuple) . (map convertToFive) . byTeam . concat . groupByFirstEl .
  sort . concat . (map convertStringtoInt) . (map words) . lines

-- make createstandings an if statement to check on input if scores are negative
