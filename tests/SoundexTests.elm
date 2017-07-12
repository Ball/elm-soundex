module SoundexTests exposing (..)

import Expect
import Fuzz exposing (int, list, string, tuple)
import Soundex exposing (..)
import String
import Test exposing (..)


aboutSoundex : Test
aboutSoundex =
    describe "Soundex.soundex"
        [ describe "Pure Examples"
            [ test "Robert = R163" <|
                \() -> Expect.equal "R163" <| soundex "Robert"
            , test "Rupert = R163" <|
                \() -> Expect.equal "R163" <| soundex "Rupert"
            , test "Rubin = R150" <|
                \() -> Expect.equal "R150" <| soundex "Rubin"
            , test "Ashcroft = Ashcraft" <|
                \() -> Expect.equal (soundex "Ashcroft") (soundex "Ashcraft")
            , test "Ashcroft = A261" <|
                \() -> Expect.equal "A261" <| soundex "Ashcroft"
            , test "Tymczak = T522" <|
                \() -> Expect.equal "T522" <| soundex "Tymczak"
            , test "Pfister = P236" <|
                \() -> Expect.equal "P236" <| soundex "Pfister"
            , test "a = A000" <|
                \() -> Expect.equal "A000" <| soundex "a"
            , test "the = T000" <|
                \() -> Expect.equal "T000" <| soundex "the"
            ]
        , describe "Starbucks Names?"
            [ test "Brian & brain" <|
                \() -> Expect.equal (soundex "Brian") (soundex "brain")
            ]
        ]
