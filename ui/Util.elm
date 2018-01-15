module Util exposing
    ( uBase
    , hexStrToInt
    , toBin
    , toBinStr
    )

{-| Convert among textual representations of numbers.
Sometimes it's useful to represent a number using a different base or radix.
Some common bases include 16 (hexadecimal), 2 (binary) and 10 (decimal), but others exist.
# Conversions
@docs base, uBase
# Convenience Functions
@docs hex, dec, oct, bin
# Interesting Alphabets
@docs bitcoin, flickr, ripple
-}

import List as L
import Result exposing (Result)
import String exposing (..)

{-| "Unsafe" Conversion
Convert a string to a base n integer, suppressing errors.  Use with caution.
    uBase 16 "100" -- 256
    uBase 16 "1ff" -- 1 (probably not what was intended!)
-}

hexStrToInt : String -> Int
hexStrToInt = uBase 16

uBase : Int -> String -> Int
uBase n str =
    let
        chars = L.filterMap (Result.toMaybe << toIntExt << fromChar) <| toList str
        len = L.length chars
        weights = L.indexedMap (\x v -> (n ^ (len - 1 - x)) * v) chars
    in
        L.foldr (+) 0 weights

toIntExt : String -> Result.Result String Int
toIntExt c =
    case c of
        "a" ->
            Ok 10
        "b" ->
            Ok 11
        "c" ->
            Ok 12
        "d" ->
            Ok 13
        "e" ->
            Ok 14
        "f" ->
            Ok 15
        _ ->
            String.toInt c

toBin n =
    case n of
        0 -> [0]
        _ -> List.reverse (toBin_ n)


toBin_ n =
    case n of
        0 -> []
        _ -> (n % 2) :: (toBin_ (n // 2))

toBinStr : Int -> String
toBinStr n = String.join "" (List.map (toString) (toBin n))
