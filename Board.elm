module Board exposing (Board, randBoard, genBoard, validWords, wordOnBoard, BoardGenSpec)

import Matrix exposing (Matrix, Location, loc, row, col, colCount, rowCount)
import Random exposing (Generator)
import List exposing (..) -- I really have to do this?
import Dict
import String

import Base exposing (..)
import Lexicon exposing (..)

type alias Board = Matrix String

randFrom : List a -> Generator a
randFrom xs = Random.map (\n -> iKnowWhatImDoing <| head <| drop n xs)
                         (Random.int 0 (length xs - 1))

randGivenProbs : List (a, Float) -> Generator a
randGivenProbs ps = let pick ps x = case ps of
                                      [] -> Debug.crash "Empty probability list!"
                                      [(a,_)] -> a
                                      (a,p)::ps' -> if x < p then a else pick ps' (x-p)
                    in Random.map (pick ps) (Random.float 0 1)

randCell : Generator String
randCell = randGivenProbs cellProbs

randBoard : Int -> Int -> Generator Board
randBoard m n = Random.list (m*n) randCell |>
                Random.map (Matrix.fromList << groups n)

adjacent : Board -> Location -> List Location
adjacent b l = map (\(r,c) -> loc (row l + r) (col l + c))
                   [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)] |>
               filter (\l -> col l >= 0 && col l < colCount b &&
                             row l >= 0 && row l < rowCount b)

allWordsAt : Board -> String -> Location -> List Location -> Lexicon -> List String
allWordsAt b head loc visited lex =
    let here = if lend lex then [head] else []
    in case ldict lex of
         Nothing -> here
         Just ld -> let nextLocs = filter (not << flip member visited) (adjacent b loc)
                    in here ++ concatMap (\l -> let ch = iKnowWhatImDoing <| Matrix.get l b
                                                in case Dict.get ch ld of
                                                     Nothing -> []
                                                     Just lex' -> allWordsAt b
                                                                             (head ++ ch)
                                                                             l
                                                                             (loc :: visited)
                                                                             lex')
                                         nextLocs

allLocs : Board -> List Location
allLocs b = concatMap (\r -> map (\c -> loc r c) [0..colCount b - 1]) [0..rowCount b - 1]

allWords : Board -> List String
allWords b = allLocs b |>
             concatMap (\l -> allWordsAt b "" l [] lexicon) |>
             sortUnique

validWords : Board -> List String
validWords b = allWords b |> filter (\s -> String.length s >= 3)

randLoc : Board -> Generator Location
randLoc b = Random.pair (Random.int 0 (rowCount b - 1)) (Random.int 0 (colCount b - 1))

-- Changes one letter randomly
mutateBoard : Board -> Generator Board
mutateBoard b = randLoc b `Random.andThen` (\l -> Random.map (\ch -> Matrix.set l ch b) randCell)

-- tfw no monads
randReturn : a -> Generator a
randReturn x = Random.map (\_ -> x) Random.bool

randBoardWithScore : Int -> Int -> Int -> Int -> Generator Board
randBoardWithScore m n s r =
    let rbws : Int -> Board -> Generator Board
        rbws s' b = if s' <= s + r && s' >= s - r
                      then randReturn b
                      else mutateBoard b
                             `Random.andThen` (\b' -> let s'' = length (validWords b')
                                                      in if abs (s'' - s) < abs (s' - s)
                                                           then rbws s'' b'
                                                           else rbws s' b)
        b0 = randBoard m n
    in b0 `Random.andThen` (\b -> rbws (length (validWords b)) b)

type alias BoardGenSpec = { m: Int, n: Int, score: Int, range: Int, seed: Int }
genBoard : BoardGenSpec -> Board
genBoard bgs = fst <| Random.step (randBoardWithScore bgs.m bgs.n bgs.score bgs.range) (Random.initialSeed bgs.seed)

nextPaths : String -> List Location -> Board -> List (List Location)
nextPaths ch path b = (case path of
                         [] -> allLocs b
                         l::_ -> adjacent b l) |>
                      filter (\l -> not (member l path)) |>
                      filter (\l -> iKnowWhatImDoing (Matrix.get l b) == ch) |>
                      map (\l -> l::path)

wordOnBoard : String -> Board -> Bool
wordOnBoard s b = case cellSplit s of
                       Nothing -> False
                       Just (h,t) -> any (\p -> wordFrom t p b) (nextPaths h [] b) -- Code duplication - can remove?

wordFrom : String -> List Location -> Board -> Bool
wordFrom left path b = case (left, path) of
                          (_,[]) -> False
                          ("",_) -> True
                          _ -> case cellSplit left of
                                 Nothing -> False
                                 Just (h,t) -> any (\p -> wordFrom t p b) (nextPaths h path b)
