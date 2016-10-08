-- Stuff that everything needs, should probably be reorganized once the structure of the project is more solid
module Base exposing (cellProbs, possibleCells, cellHead, cellSplit, iKnowWhatImDoing)

import List exposing (..)
import String

cellProbs = [
    ("a", 0.0817),
    ("b", 0.0149),
    ("c", 0.0278),
    ("d", 0.0425),
    ("e", 0.1270),
    ("f", 0.0223),
    ("g", 0.0202),
    ("h", 0.0609),
    ("i", 0.0697),
    ("j", 0.0015),
    ("k", 0.0077),
    ("l", 0.0402),
    ("m", 0.0241),
    ("n", 0.0675),
    ("o", 0.0751),
    ("p", 0.0193),
    ("qu", 0.0010),
    ("r", 0.0599),
    ("s", 0.0633),
    ("t", 0.0906),
    ("u", 0.0276),
    ("v", 0.0098),
    ("w", 0.0236),
    ("x", 0.0015),
    ("y", 0.0197),
    ("z", 0.0007)
    ]

possibleCells = map fst cellProbs

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

iKnowWhatImDoing : Maybe a -> a
iKnowWhatImDoing x = case x of
                       Just x -> x
                       _ -> Debug.crash "Apparently not..."
