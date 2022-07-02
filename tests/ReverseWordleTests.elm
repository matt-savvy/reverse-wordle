module ReverseWordleTests exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Random
import ReverseWordle exposing (..)
import Test exposing (..)
import Words exposing (masterList)


createWordDictTests : Test
createWordDictTests =
    describe "createWordDict"
        [ test "gets the right count" <|
            \_ ->
                Expect.equal
                    (createWordDict "radar")
                    (Dict.fromList
                        [ ( 'r', 2 )
                        , ( 'a', 2 )
                        , ( 'd', 1 )
                        ]
                    )
        ]


filteredWords : Array Word
filteredWords =
    let
        doesntContain : String -> String -> Bool
        doesntContain substr str =
            not (String.contains substr str)
    in
    Array.fromList masterList
        |> Array.filter (doesntContain "ight")


getWord : Int -> String
getWord i =
    Array.get i filteredWords
        |> Maybe.withDefault "plane"


solverTests : Test
solverTests =
    let
        getLastGuess : Array Guess -> Word
        getLastGuess guesses =
            Array.get (Array.length guesses - 1) guesses
                |> (\maybeGuess ->
                        case maybeGuess of
                            Just (Guess word _) ->
                                word

                            _ ->
                                ""
                   )
    in
    describe "solver tests"
        [ fuzz2 (Fuzz.intRange 0 (Array.length filteredWords)) Fuzz.int "last guess is the word input" <|
            \i seed ->
                let
                    word : String
                    word =
                        getWord i
                in
                Expect.equal
                    (getLastGuess (solve (createPuzzle word) (Random.initialSeed seed)))
                    word
        ]
