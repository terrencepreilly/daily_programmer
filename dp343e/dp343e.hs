{- https://www.reddit.com/r/dailyprogrammer/comments/7hhyin/20171204_challenge_343_easy_major_scales/

Given a major scale name and a name in the Soflège system,
give the corresponding note.

Example Usage:

    Prelude> soflegeOf "C" "Do"
    "C"
    Prelude> soflegeOf "D#" "Mi"
    "G"

 -}
import Data.List

type Note = String
type Scale = [Note]

_scale :: Scale
_scale = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

_majorIndices :: [Int]
_majorIndices = [0, 2, 4, 5, 9, 11]

_soflege :: [String]
_soflege = ["Do", "Re", "Mi", "Fa", "So", "La", "Ti"]

majorScale :: Note -> Scale
majorScale note =
    let
        i = case findIndex (==note) _scale of
                Just index -> index
                Nothing -> -1
        scale = drop i $ cycle _scale
    in
        map (\x -> scale !! x) _majorIndices

soflegeOf :: Note -> String -> String
soflegeOf note soflege =
    let
        scale = majorScale note
        i = case findIndex (==soflege) _soflege of
            Just index -> index
            Nothing -> -1
    in
        scale !! i

main :: IO ()
main = interact id
