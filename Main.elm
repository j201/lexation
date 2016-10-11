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

type alias Model = { board: Board, typed: String, scored: List String }

initial : Model
initial = { board = boardBySeed 0, typed = "", scored = [] }

boardView : Model -> Html msg
boardView m =
    table [] (map (\r ->
        tr [] (map (\c -> td [] [text c]) r))
                  (Matrix.toList m.board))

update : String -> Model -> Model
update s m = if String.length s >= 3 && not (member s m.scored) && hasWord s lexicon && wordOnBoard s m.board
               then { m | typed = "", scored = s::m.scored }
               else { m | typed = s }

view : Model -> Html String
view m =
    div [] [
        boardView m,
        input [value m.typed, onInput identity] [],
        ul [] (map (\s -> li [] [text s]) m.scored)
    ]

main = App.beginnerProgram { model = initial, update = update, view = view }
