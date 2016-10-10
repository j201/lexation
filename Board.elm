module Board exposing (Board, randBoard, boardBySeed, allWords)

import Matrix exposing (Matrix, Location, loc, row, col, colCount, rowCount)
import Random exposing (Generator)
import List exposing (..) -- I really have to do this?
import Dict

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

randBoard : Int -> Int -> Generator Board
randBoard m n = Random.list (m*n) (randGivenProbs cellProbs) |>
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

allWords : Board -> List String
allWords b = sortUnique <|
             concatMap (\l -> allWordsAt b "" l [] lexicon) <|
             concatMap (\r -> map (\c -> loc r c) [0..colCount b]) [0..rowCount b]

boardBySeed n = fst <| Random.step (randBoard 4 4) (Random.initialSeed n)
