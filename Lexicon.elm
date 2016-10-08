module Lexicon exposing (Lexicon, lexicon, hasWord, addWord, ldict, lend)

import Dict exposing (Dict)
import String
import List exposing (..)

import Words exposing (words)
import Base exposing (..)

type Lexicon = Lexicon Bool (Maybe (Dict String Lexicon))

ldict (Lexicon _ d) = d
lend (Lexicon b _) = b
lempty = Lexicon False Nothing

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
