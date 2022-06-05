module ReverseWordleTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ReverseWordle exposing (..)
import Test exposing (..)


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
