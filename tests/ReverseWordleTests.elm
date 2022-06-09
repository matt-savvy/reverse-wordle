module ReverseWordleTests exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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


arrayWords : Array String
arrayWords =
    Array.fromList masterList


getWord : Int -> String
getWord i =
    Array.get i arrayWords
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
        -- [ fuzz (Fuzz.intRange 0 (Array.length arrayWords)) "last guess is the word input"  <|
        --     \i ->
        --         let
        --             word : String
        --             word = getWord i
        --         in
        --         Expect.equal
        --             (getLastGuess (solve (createPuzzle word)))
        --             word
        -- , test "tight" <|
        [ test "tight" <|
            \_ ->
                Expect.equal
                    (getLastGuess (solve (createPuzzle "tight")))
                    "tight"

        -- , test "right" <|
        --     \_ ->
        --     Expect.equal
        --             (getLastGuess (solve (createPuzzle "right")))
        --             "right"
        ]
