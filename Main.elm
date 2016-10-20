module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Matrix
import String

import Lexicon exposing (..)
import Board exposing (..)

type alias Model = { board: Board, typed: String, scored: List String, bgs: BoardGenSpec }

type Msg =
    Input String |
    NewBoard

boardGenSpec : BoardGenSpec
boardGenSpec = { m = 4, n = 4, score = 100, range = 3, seed = 0 }

initial : Model
initial = { board = genBoard boardGenSpec, typed = "", scored = [], bgs = boardGenSpec }

boardView : Model -> Html msg
boardView m =
    table [] (map (\r ->
        tr [] (map (\c -> td [] [text c]) r))
                  (Matrix.toList m.board))

update : Msg -> Model -> Model
update msg m = case msg of
                 Input s -> if String.length s >= 3 && not (member s m.scored) && hasWord s lexicon && wordOnBoard s m.board
                              then { m | typed = "", scored = s::m.scored }
                              else { m | typed = s }
                 NewBoard -> let oldBgs = m.bgs -- it is silly that i have to do this
                                 newBgs = { oldBgs | seed = m.bgs.seed + 1 }
                             in { m | board = genBoard newBgs, bgs = newBgs }

view : Model -> Html Msg
view m =
    div [] [
        boardView m,
        button [onClick NewBoard] [text "New board"],
        input [value m.typed, onInput Input] [],
        ul [] (map (\s -> li [] [text s]) m.scored)
    ]

main = App.beginnerProgram { model = initial, update = update, view = view }
