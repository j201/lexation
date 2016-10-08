-- Stuff that everything needs, should probably be reorganized once the structure of the project is more solid
module Base exposing (possibleCells, cellHead, cellSplit)

import String

possibleCells = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "qu", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

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
