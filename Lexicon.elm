module Lexicon exposing (Lexicon, lexicon, hasWord, addWord)

import Dict exposing (Dict)
import String
import List exposing (..)

import WordsSmall exposing (words)
import Base exposing (..)

type Lexicon = Lexicon Bool (Maybe (Dict String Lexicon))

ldict (Lexicon _ d) = d
lend (Lexicon b _) = b
lempty = Lexicon False Nothing

cellHead : String -> Maybe String
cellHead s = let cellHead' s cs = case cs of
                                    [] -> Nothing
                                    c::cs -> if String.startsWith c s
                                                then Just c
                                                else cellHead' s cs
             in cellHead' s possibleCells

cellSplit : String -> Maybe (String, String)
cellSplit s = cellHead s |>
              Maybe.map (\h -> (h, String.dropLeft (String.length h) s))

dictCreateUpdate : Maybe (Dict comparable v) -> comparable -> v -> Dict comparable v
dictCreateUpdate d k v = case d of
                           Nothing -> Dict.singleton k v
                           Just d' -> Dict.insert k v d'

addWord : String -> Lexicon -> Lexicon
addWord s l = if s == "" then Lexicon True (ldict l)
              else case cellSplit s of
                     Nothing -> l
                     Just (h, rest) -> case ldict l of
                                         Nothing -> Lexicon (lend l) <| Just <| Dict.singleton h <| addWord rest <| lempty
                                         Just ld -> Lexicon (lend l) <| Just <|
                                                    Dict.update h (\v -> case v of
                                                                           Nothing -> Just (addWord rest lempty)
                                                                           Just l' -> Just (addWord rest l'))
                                                                  ld

hasWord : String -> Lexicon -> Bool
hasWord s l = if s == "" then lend l
              else case (ldict l, cellSplit s) of
                     (Nothing, _) -> False
                     (_, Nothing) -> False
                     (Just ld, Just (h, rest))-> case Dict.get h ld of
                                                   Nothing -> False
                                                   Just l' -> hasWord rest l'


lexicon : Lexicon
lexicon = foldr addWord lempty words
