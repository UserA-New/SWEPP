-- The project is completed but it produces false results due to false parsing of FES-String.

-- module (NICHT ÄNDERN!)
module XiangqiBot
    ( getMove
    , listMoves
    ) 
    where

import Data.Char
-- More modules may be imported
import Util
import Data.List
import Control.Monad

--- external signatures (NICHT ÄNDERN!)
getMove :: String -> String
getMove _ = "" 


listMoves :: String -> String
listMoves = theLastRepresentation 


{-
    Der Startzustand des Spiels ist Folgendes:
        "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"

    Note: But use always the string or the list without forwards slashes and the numbers in it must be represented by 1s.

    Xiangqi:

    a9 -- b9 -- c9 -- d9 -- e9 -- f9 -- g9 -- h9 -- i9    whitespace    next player
    81 -- 82 -- 83 -- 84 -- 85 -- 86 -- 87 -- 88 -- 89 --     90     --     91     

    a8 -- b8 -- c8 -- d8 -- e8 -- f8 -- g8 -- h8 -- i8
    72 -- 73 -- 74 -- 75 -- 76 -- 77 -- 78 -- 79 -- 80

    a7 -- b7 -- c7 -- d7 -- e7 -- f7 -- g7 -- h7 -- i7
    63 -- 64 -- 65 -- 66 -- 67 -- 68 -- 69 -- 70 -- 71

    a6 -- b6 -- c6 -- d6 -- e6 -- f6 -- g6 -- h6 -- i6
    54 -- 55 -- 56 -- 57 -- 58 -- 59 -- 60 -- 61 -- 62

    a5 -- b5 -- c5 -- d5 -- e5 -- f5 -- g5 -- h5 -- i5
    45 -- 46 -- 47 -- 48 -- 49 -- 50 -- 51 -- 52 -- 53
       --    --    --    --    --    --    --    --   
       --    --    --    --    --    --    --    --   (River)
       --    --    --    --    --    --    --    --
    a4 -- b4 -- c4 -- d4 -- e4 -- f4 -- g4 -- h4 -- i4
    36 -- 37 -- 38 -- 39 -- 40 -- 41 -- 42 -- 43 -- 44

    a3 -- b3 -- c3 -- d3 -- e3 -- f3 -- g3 -- h3 -- i3
    27 -- 28 -- 29 -- 30 -- 31 -- 32 -- 33 -- 34 -- 35

    a2 -- b2 -- c2 -- d2 -- e2 -- f2 -- g2 -- h2 -- i2
    18 -- 19 -- 20 -- 21 -- 22 -- 23 -- 24 -- 25 -- 26

    a1 -- b1 -- c1 -- d1 -- e1 -- f1 -- g1 -- h1 -- i1
    9  -- 10 -- 11 -- 12 -- 13 -- 14 -- 15 -- 16 -- 17

    a0 -- b0 -- c0 -- d0 -- e0 -- f0 -- g0 -- h0 -- i0
    0  -- 1  -- 2  -- 3  -- 4  -- 5  -- 6  -- 7  -- 8
-}


{-
    |             |
    | FIRST STEPS |
    |             |
-}


startState :: String
startState = "rheagaehr/9/1c5c1/s2s1s2s/9/9/S2S1S2S/1C5C1/4s4/RHEAGAEHR r"
    
-- "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r"


{-
    The function converts a whole number greater than 1
    into a series of 1s corresponding to that number in a string!
    Note: Tested!
-}
representedByOnes :: Char -> String
representedByOnes charNum
    | charNum == '9' = "111111111"
    | charNum == '8' = "11111111"
    | charNum == '7' = "1111111"
    | charNum == '6' = "111111"
    | charNum == '5' = "11111"
    | charNum == '4' = "1111"
    | charNum == '3' = "111"
    | charNum == '2' = "11"
    | otherwise = [charNum]


{-
    -> Step 1!
    The function returns a string, 
    in which whole numbers are converted into a series of 1s corresponding to that number.
    Note: Tested!
-}
fensWOnes :: String -> String
fensWOnes fens = concat(map representedByOnes fens)


startStateWOnes :: String -> String
startStateWOnes = fensWOnes


{-
    -> Step 2!
    In order to use the (mod 9), we have to get rid of forward slashes!
    The Fuction return a string with the forward slashes in it.
    Note: Tested!
-}
fensWoFs :: String -> String
fensWoFs fens = concat(splitOn '/' fens)


startStateWOnesWoFs :: String -> String
startStateWOnesWoFs f = fensWoFs (startStateWOnes f)


{-
    -> Step 3!
    The function converts a string into a list with the elements in it.
    Note: Tested!
-}
strToList :: String -> [String]
strToList fens = map (:[]) fens


startStateWOnesWoFsList :: String -> [String]
startStateWOnesWoFsList f = strToList (startStateWOnesWoFs f)


{-
    fenL = startStateWOnesWoFsList
-}
fenL :: String -> [String]
fenL = startStateWOnesWoFsList

{-
    |                |
    | FUNCTION CALLS |
    |                |
-}


{-
    The function ... all the possible moves for a given FENList.
-}
listAllMovesFunc :: [[Char]] -> [[(Int, Int)]]
listAllMovesFunc fenL = map auxFunc [0..89] where
    auxFunc index = listMovesFunc fenL index 


{-
    The function ... the possible moves for an individual piece.
-}
listMovesFunc :: [[Char]] -> Int -> [(Int, Int)]
listMovesFunc fenL indexFrom = map auxFuncTwo [x | x <- [0..89], x /= indexFrom] where
    auxFuncTwo indexTo = if isPieceInRedTeam fenL indexFrom && canMove fenL indexFrom indexTo && not (isDeathStareOn (movePiece fenL indexFrom indexTo)) then (indexFrom, indexTo)
                            else (-1,-1)


{-
    |                     |
    | CEDRIC'S FUNCTIONS  |
    |                     |
-}

checkMate :: [String] -> (Int,Int) -> Bool
checkMate fenL (from,to) =
    let board = movePiece fenL from to in
        any (\(f,t) -> t == (findGeneral board (fenL !! 91))) (listAllMovesFlat board)

listAllMovesFlat :: [[Char]] -> [(Int, Int)]
listAllMovesFlat fenL = filter (/= (-1,-1)) a where
    a = concat (listAllMovesFunc fenL)

getMoveDesc :: (Int, Int) -> [Char]
getMoveDesc (from, to) = whereIsIt from ++ "-" ++ whereIsIt to

getAllLegalMoves :: [[Char]] -> [(Int, Int)]
-- getAllLegalMoves fenL = (listAllMovesFlat fenL)
getAllLegalMoves fenL = filter (not . checkMate fenL) (listAllMovesFlat fenL)

representingFunc :: String -> [[Char]]
representingFunc s = map getMoveDesc (getAllLegalMoves (fenL s))

theLastRepresentation :: String -> [Char]
theLastRepresentation f = filter (\c -> c /= '\\' && c /= '\"') (show (representingFunc f))


{-
    |         |
    | FIGURES |
    |         |
-}


{-
    The function returns True, if the soldier can move, otherwise False.
-}
canSoldierMove :: [String] -> Int -> Int -> Bool -> Bool
canSoldierMove fenL indexFrom indexTo isRed =
    {-if not isRed then -- If the soldier plays for red team
        -- Before the river (and) takes a step forward (and) stays still on the same column (and) (the end point is "1" (or) there's a black piece on the end point) (or)
        getY indexFrom <= 4 && getY indexTo -  getY indexFrom == 1 && getX indexTo == getX indexFrom && (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo))) ||
        -- The river crossed (and) takes a step forward (and) stays still on the same column (and) (the end point is "1" (or) there's a black piece on the end point) (or)
        5 <= getY indexFrom && getY indexTo -  getY indexFrom == 1 && getX indexTo == getX indexFrom && (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo))) ||
        -- The river crossed (and) takes a step sideways (and) stays still on the same row (and) (the end point is "1" (or) there's a black piece on the end point)
        5 <= getY indexFrom && getY indexTo == getY indexFrom && abs (getX indexTo - getX indexFrom) == 1 && (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo)))
    else -- If the soldier plays for black team
        -- Before the river (and) takes a step forward (and) stays still on the same column (and) (the end point is "1" (or) there's a red piece on the end point) (or) 
        5 <= getY indexFrom && getY indexFrom - getY indexTo == 1 && getX indexTo == getX indexFrom && (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))  ||
        -- The river crossed (and) takes a step forward (and) stays still on the same column (the end point is "1" (or) there's a red piece on the end point) (or) 
        getY indexFrom <= 4 && getY indexFrom - getY indexTo == 1 && getX indexTo == getX indexFrom  && (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo))) ||
        -- The river crossed (and) takes a step sideways (and) stays still on the same row (and) (the end point is "1" (or) there's a red piece on the end point)
        getY indexFrom <= 4 && getY indexTo == getY indexFrom && abs (getX indexTo - getX indexFrom) == 1  && (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))
-}
    let sidewalk = (isRed && getY indexFrom < 5) || (not isRed && getY indexFrom >= 5) in
        if isRed then
            ((getY indexTo - getY indexFrom == -1 && getX indexFrom == getX indexTo) || (
                sidewalk && getY indexTo == getY indexFrom && abs (getX indexTo - getX indexFrom) == 1
            )) && (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))
        else
            ((getY indexTo - getY indexFrom == 1 && getX indexFrom == getX indexTo) || (
                sidewalk && getY indexTo == getY indexFrom && abs (getX indexTo - getX indexFrom) == 1
            )) && (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo)))

{-
    The function returns True, if the advisor can move, otherwise False.
-}
canAdvisorMove :: [String] -> Int -> Int -> Bool -> Bool 
canAdvisorMove fenL indexFrom indexTo isRed =
    if not isRed then -- If the advisor plays for red team
        -- A move inside the palace (and)
        3 <= getX indexTo && getX indexTo <= 5 && 0 <= getY indexTo && getY indexTo <= 2 &&
        -- takes a step diagonally (and)
        abs (getY indexTo - getY indexFrom) == 1 && abs (getX indexTo - getX indexFrom) == 1 &&
        -- (the end point is "1" (or) there's a black piece on the end point)
        (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo)))
    else -- If the advisor plays for black team
        -- A move inside the palace (and)
        3 <= getX indexTo && getX indexTo <= 5 && 7 <= getY indexTo && getY indexTo <= 9 &&
        -- takes a step diagonally (and)
        abs (getY indexTo - getY indexFrom) == 1 && abs (getX indexTo - getX indexFrom) == 1 &&
        -- (the end point is "1" (or) there's a red piece on the end point)
        (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))


{-
    The function returns True, if the elephant can move, otherwise False.
-}
canElephantMove :: [String] -> Int -> Int -> Bool -> Bool
canElephantMove fenL indexFrom indexTo isRed =
    if not isRed then -- if the elephant plays for red team
        -- A move before the river (and) takes two steps diagonally (and)
        getY indexTo <= 4 && abs (getY indexTo - getY indexFrom) == 2 && abs (getX indexTo - getX indexFrom) == 2 &&
        -- On the midpoint, there's no piece (and)
        let x = (getX indexTo + getX indexFrom) `div` 2
            y = (getY indexTo + getY indexFrom) `div` 2 in
                let indexBetween = getIndex (x, y) in
                    fenL !! indexBetween == "1" &&
                    -- (the end point is "1" (or) there's a black piece on the end point)
                    (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo)))
    else -- if the elephant plays for black team
        -- A move before the river (and) takes two steps diagonally (and)
        5 <= getY indexTo && abs (getY indexTo - getY indexFrom) == 2 && abs (getX indexTo - getX indexFrom) == 2 &&
        -- On the midpoint, there's no piece (and)
        let x = (getX indexTo + getX indexFrom) `div` 2
            y = (getY indexTo + getY indexFrom) `div` 2 in
                let indexBetween = getIndex (x, y) in
                    fenL !! indexBetween == "1" &&
                    -- (the end point is "1" (or) there's a red piece on the end point)
                    (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))


{-
    The function returns True, if the general can move, otherwise False.
-}
canGeneralMove :: [String] -> Int -> Int -> Bool -> Bool
canGeneralMove fenL indexFrom indexTo isRed =
    if not isRed then -- If the general plays for red team
        -- A move inside the palace (and) takes a step vertically (and) stays still on the same column (and)
        3 <= getX indexTo && getX indexTo <= 5 && 0 <= getY indexTo && getY indexTo <= 2 &&
        abs(getY indexTo -  getY indexFrom) == 1 && getX indexTo == getX indexFrom &&
        -- (the end point is "1" (or) there's a black piece on the end point) (or)
        (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo))) ||
        -- A move inside the palace (and) takes a step sideways (and) stays still on the same row (and)
        3 <= getX indexTo && getX indexTo <= 5 && 0 <= getY indexTo && getY indexTo <= 2 &&
        abs(getX indexTo -  getX indexFrom) == 1 && getY indexTo == getY indexFrom &&
        -- (the end point is "1" (or) there's a black piece on the end point)
        (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo)))
    else
        -- A move inside the palace (and) takes a step vertically (and) stays still on the same column
        3 <= getX indexTo && getX indexTo <= 5 && 7 <= getY indexTo && getY indexTo <= 9 &&
        abs(getY indexTo -  getY indexFrom) == 1 && getX indexTo == getX indexFrom &&
        -- (the end point is "1" (or) there's a red piece on the end point) (or)
        (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo))) ||
        -- A move inside the palace (and) takes a step sideways (and) stays still on the same row
        3 <= getX indexTo && getX indexTo <= 5 && 7 <= getY indexTo && getY indexTo <= 9 &&
        abs(getX indexTo -  getX indexFrom) == 1 && getY indexTo == getY indexFrom &&
        -- (the end point is "1" (or) there's a red piece on the end point) (or)
        (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))


{-
    The function returns True, if the horse can move, otherwise False.
-}
canHorseMove :: [String] -> Int -> Int -> Bool -> Bool
canHorseMove fenL indexFrom indexTo isRed =
    -- Horizontal
    (abs (getX indexFrom - getX indexTo) == 2 && abs (getY indexFrom - getY indexTo) == 1 &&
    let x = (getX indexTo + getX indexFrom) `div` 2
        y = getY indexFrom in
            let indexBetween = getIndex (x, y) in
                fenL !! indexBetween == "1" &&
                (if isRed then
                    -- (the end point is "1" (or) there's a black piece on the end point)
                    (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))
                    else
                        -- (the end point is "1" (or) there's a red piece on the end point)
                        (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo))))) ||
    -- Vertical
    (abs (getY indexFrom - getY indexTo) == 2 && abs (getX indexFrom - getX indexTo) == 1 &&
    let x = getX indexFrom
        y = (getY indexTo + getY indexFrom) `div` 2 in
            let indexBetween = getIndex (x, y) in
                fenL !! indexBetween == "1" &&
                if isRed then
                    -- (the end point is "1" (or) there's a black piece on the end point)
                    (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))
                    else
                        -- (the end point is "1" (or) there's a red piece on the end point)
                        (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo))))


{-
    The function returns True, if the rook can move, otherwise False.
-}
canRookMove :: [String] -> Int -> Int -> Bool -> Bool 
canRookMove fenL indexFrom indexTo isRed =
    (numberOfPiecesOnTheHorizontalLine fenL indexFrom indexTo 0  || numberOfPiecesOnTheVerticalLine fenL indexFrom indexTo 0) &&
    if isRed then
        -- (the end point is "1" (or) there's a black piece on the end point)
        (fenL !! indexTo == "1" || isLower (head (fenL !! indexTo)))
        else
            -- the end point is "1" (or) there's a red piece on the end point)
            (fenL !! indexTo == "1" || isUpper (head (fenL !! indexTo)))

{-
    The function returns True, if the cannon can move, otherwise False.
-}
canCannonMove :: [String] -> Int -> Int -> Bool -> Bool
canCannonMove fenL indexFrom indexTo isRed =
    (numberOfPiecesOnTheHorizontalLine fenL indexFrom indexTo 0  || numberOfPiecesOnTheVerticalLine fenL indexFrom indexTo 0 ) &&
    fenL !! indexTo == "1" ||
    (numberOfPiecesOnTheHorizontalLine fenL indexFrom indexTo 1  || numberOfPiecesOnTheVerticalLine fenL indexFrom indexTo 1 ) &&
    fenL !! indexTo /= "1" &&
    if isRed then
        -- there's a black piece on the end point
        isLower (head (fenL !! indexTo))
        else
            -- there's a red piece on the end point
            isUpper (head (fenL !! indexTo))



{-
    |                     |
    | NECESSARY FUNCTIONS |
    |                     |
-}


{-
    The function return the number of pieces between the start point and the end point on a vertical line. But it doesn't include the start point and end point.
-}
numberOfPiecesOnTheVerticalLine :: [String] -> Int -> Int -> Int -> Bool 
numberOfPiecesOnTheVerticalLine fenL indexFrom indexTo expectedLength =
    -- This function is for both teams.
    ((getY indexFrom < getY indexTo) || (getY indexTo < getY indexFrom)) && getX indexTo == getX indexFrom &&
        let pathRook = [ (x, y) | x <- [getX indexTo], y <- [(min (getY indexFrom) (getY indexTo) + 1)..(max (getY indexFrom) (getY indexTo) - 1)]] in
            let pathRookIndices = map getIndex pathRook in
                expectedLength == length (filter (\i -> (fenL !! i) /= "1") pathRookIndices)


{-
    The function return the number of pieces between the start point and the end point on a horizontal line. But it doesn't include the start point and end point.
-}
numberOfPiecesOnTheHorizontalLine :: [String] -> Int -> Int -> Int -> Bool
numberOfPiecesOnTheHorizontalLine fenL indexFrom indexTo expectedLength =
    -- This function is for both teams.
    ((getX indexFrom < getX indexTo) || (getX indexTo < getX indexFrom)) && getY indexTo == getY indexFrom &&
        let pathRook = [ (x, y) | x <- [(min (getX indexFrom) (getX indexTo) + 1)..(max (getX indexFrom) (getX indexTo) - 1)], y <- [getY indexTo]] in
            let pathRookIndices = map getIndex pathRook in
                expectedLength == length (filter (\i -> (fenL !! i) /= "1") pathRookIndices)


{-
    The function returns True, if Death Stare is on.
-}
isDeathStareOn :: [String] -> Bool
isDeathStareOn fenL =
    let generalRedIndex = removeMaybe (elemIndex "G" fenL)
        generalBlackIndex = removeMaybe (elemIndex "g" fenL) in
            getX generalRedIndex == getX generalBlackIndex &&
            numberOfPiecesOnTheVerticalLine fenL generalRedIndex generalBlackIndex 0

{-
    The function returns a list in which a move is happened.
-}
movePiece :: [String] -> Int -> Int -> [String]
movePiece fenL indexFrom indexTo = 
    let firstChangeL = replaceAtIndex indexTo (fenL !! indexFrom) fenL in
        let secondChangeL =  replaceAtIndex indexFrom "1" firstChangeL in
            let finalChangeL = replaceAtIndex 91 (if fenL !! 91 == "r" then "b" else "r") secondChangeL in
                    finalChangeL

{-
    This function return True or False, depends on the move.
-}
canMove :: [String] -> Int -> Int -> Bool 
canMove fenL indexFrom indexTo =
  case fenL !! indexFrom of
    "G" -> canGeneralMove  fenL indexFrom indexTo True
    "g" -> canGeneralMove  fenL indexFrom indexTo False
    "A" -> canAdvisorMove  fenL indexFrom indexTo True
    "a" -> canAdvisorMove  fenL indexFrom indexTo False
    "E" -> canElephantMove fenL indexFrom indexTo True
    "e" -> canElephantMove fenL indexFrom indexTo False
    "H" -> canHorseMove    fenL indexFrom indexTo True
    "h" -> canHorseMove    fenL indexFrom indexTo False
    "R" -> canRookMove     fenL indexFrom indexTo True
    "r" -> canRookMove     fenL indexFrom indexTo False
    "C" -> canCannonMove   fenL indexFrom indexTo True
    "c" -> canCannonMove   fenL indexFrom indexTo False
    "S" -> canSoldierMove  fenL indexFrom indexTo True
    "s" -> canSoldierMove  fenL indexFrom indexTo False
    _   -> False


{-
    |                     |
    | AUXILIARY FUNCTIONS |
    |                     |
-}


{-
    The function converts an index to the corresponding coordinates.
    An example: *Main> getPosition 87
             (6,9)
    Note: Tested!
-}
getPosition :: Int -> (Int, Int)
getPosition index = (index `mod` 9, index `div` 9)


{-
    The function is the inverse function of getPosition.
    An example: *Main> getIndex (6,9)
                87
    Note: Tested!
-}
getIndex :: (Int, Int) -> Int
getIndex (x,y) = x + y * 9


{-
    The function returns the x coordinate of an index.
    An example: *Main> getX 87
                6
    Note: Tested!
-}
getX :: Int  -> Int 
getX index = fst (getPosition index)


{-
    The function returns the y coordinate of an index.
    An example: *Main> getY 87
                9
    Note: Tested!
-}
getY :: Int -> Int
getY index = snd (getPosition index)


{-
    The function converts Just Int to Int.
    Note: Tested!
-}
removeMaybe :: Maybe Int -> Int
removeMaybe Nothing = 40
removeMaybe (Just i) = i


{-
    The function replaces the element at the given index in a list.
    Note: Tested!
-}
replaceAtIndex :: Int -> String -> [String] -> [String]    
replaceAtIndex index x xs = take index xs ++ [x] ++ drop (index+1) xs


{-
    Index positions of a string to which the functions fensWOnes, fensWoFs and strToList are applied are the following:
    90 -> whitespace    
    91 -> shows the player whose turn it's.

    The function converts the given index into a position of form [Column] [Row], i.e c3, d7, e2, etc.
    Note: Tested!
-}
whereIsIt :: Int -> String
whereIsIt index = letterPart index ++ numberPart index


{-
    The function returns the column of a position.
    Note: index >= 89
    Note: Tested!
-}
letterPart :: Int -> String
letterPart index
    | index `mod` 9 == 0 = "a"
    | index `mod` 9 == 1 = "b"
    | index `mod` 9 == 2 = "c"
    | index `mod` 9 == 3 = "d"
    | index `mod` 9 == 4 = "e"
    | index `mod` 9 == 5 = "f"
    | index `mod` 9 == 6 = "g"
    | index `mod` 9 == 7 = "h"
    | index `mod` 9 == 8 = "i"


{-
    The function returns the row of a position.
    Note: index >= 89
    Note: Tested!
-}
numberPart :: Int -> String
numberPart index = show (index `div` 9)

{-
    The function return the index of the general of the corresponding team.
    Note: Tested!
-}
findGeneral :: [[Char]] -> [Char] -> Int
findGeneral fenL team =
    removeMaybe (elemIndex general fenL) where
        general = if team == "r" then "G" else "g"


{-
    The function extract the last element of a list that consists of strings, that means it returns the next player.
    Note: Tested!
-}
isRed :: [String] -> Bool
isRed fenL
    | last fenL == "r" = False
    | last fenL == "b" = True


{-
    The function return true, if the player playing now is from red team.
-}
isPieceInRedTeam :: [[Char]] -> Int -> Bool
isPieceInRedTeam fenL indexFrom =
    if isRed fenL then
        not (isDigit (head (fenL !! indexFrom))) && isUpper (head (fenL !! indexFrom))
        else
            not (isDigit (head (fenL !! indexFrom))) && isLower (head (fenL !! indexFrom))
