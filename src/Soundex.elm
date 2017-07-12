module Soundex exposing (Soundex, soundex)

import Regex


type alias Soundex =
    String


soundex : String -> Soundex
soundex word =
    ( String.toUpper <| String.left 1 word, String.toLower word )
        |> removeHandW
        |> consonantsToDigits
        |> removeAdjacentDigits
        |> removeVowelsAfterFirst
        |> setFirstCharacter
        |> paddOrTrimToThreeDigits


removeHandW : ( String, String ) -> ( String, String )
removeHandW =
    Tuple.mapSecond (Regex.replace Regex.All (Regex.regex "[hw]") (\_ -> ""))


consonantToDigit : Char -> Char
consonantToDigit a =
    if List.member a [ 'b', 'f', 'p', 'v' ] then
        '1'
    else if List.member a [ 'c', 'g', 'j', 'k', 'q', 's', 'x', 'z' ] then
        '2'
    else if List.member a [ 'd', 't' ] then
        '3'
    else if 'l' == a then
        '4'
    else if List.member a [ 'm', 'n' ] then
        '5'
    else if 'r' == a then
        '6'
    else
        a


consonantsToDigits : ( String, String ) -> ( String, String )
consonantsToDigits =
    Tuple.mapSecond (\body -> body |> String.toList |> List.map consonantToDigit |> String.fromList)


removeAdjacentDigits : ( String, String ) -> ( String, String )
removeAdjacentDigits =
    let
        rdcr : Char -> ( Char, List Char ) -> ( Char, List Char )
        rdcr current ( prev, acc ) =
            if prev /= current then
                ( current, current :: acc )
            else
                ( prev, acc )
    in
    Tuple.mapSecond
        (\body ->
            body
                |> String.toList
                |> List.foldr rdcr ( ' ', [] )
                |> Tuple.second
                |> String.fromList
        )


removeVowelsAfterFirst : ( String, String ) -> ( String, String )
removeVowelsAfterFirst =
    Tuple.mapSecond
        (Regex.replace Regex.All
            (Regex.regex "[aeiouy]")
            (\match ->
                if match.index == 0 then
                    match.match
                else
                    ""
            )
        )


setFirstCharacter : ( String, String ) -> String
setFirstCharacter ( lead, a ) =
    lead ++ String.dropLeft 1 a


paddOrTrimToThreeDigits : String -> String
paddOrTrimToThreeDigits a =
    a
        ++ "000"
        |> String.left 4
