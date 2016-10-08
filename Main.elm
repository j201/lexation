import Matrix exposing (Matrix, Location, loc, row, col, colCount, rowCount)
import Random exposing (Generator)
import List exposing (..) -- I really have to do this?
import Dict

import Base exposing (..)
import Lexicon exposing (..)

type alias Board = Matrix String

iKnowWhatImDoing : Maybe a -> a
iKnowWhatImDoing x = case x of
                       Just x -> x
                       _ -> Debug.crash "Apparently not..."

randFrom : List a -> Generator a
randFrom xs = Random.map (\n -> iKnowWhatImDoing <| head <| drop n xs)
                         (Random.int 0 (length xs - 1))

groups : Int -> List a -> List (List a)
groups n xs = if isEmpty xs
                then []
                else take n xs :: groups n (drop n xs)

randBoard : Int -> Int -> Generator Board
randBoard m n = Random.list (m*n) (randFrom possibleCells) |>
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

sortUnique : List comparable -> List comparable
sortUnique = let uniq xs = case xs of
                             [] -> []
                             x::[] -> [x]
                             x::y::xs -> if x == y then uniq (y::xs) else x :: uniq (y::xs)
             in uniq << sort

allWords : Board -> List String
allWords b = sortUnique <|
             concatMap (\l -> allWordsAt b "" l [] lexicon) <|
             concatMap (\r -> map (\c -> loc r c) [0..colCount b]) [0..rowCount b]
