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

findMinLength:: [String] -> Bool
findMinLength a =
  if length a /= 4 then True
  else False

lengthNotFour:: [[String]] -> Bool
lengthNotFour a =
  if or (map findMinLength a) then True
  else False

err = "ERROR"

createStandings:: String -> String
createStandings str =
  if ((lengthNotFour . (map words) . lines) str) then err
  else (show . (map words) . lines) str

-- make createstandings an if statement to check on input if scores are negative
