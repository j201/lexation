import Matrix exposing (Matrix, Location, loc, row, col)
import Random exposing (Generator)
import List exposing (..) -- I really have to do this?

import Base exposing (..)

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
