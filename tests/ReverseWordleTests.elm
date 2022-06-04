module ReverseWordleTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ReverseWordle exposing (..)
import Test exposing (..)


getFeedbackTestSuite : Test
getFeedbackTestSuite =
    describe "getFeedback"
        [ test "no hits" <|
            \_ ->
                getFeedback (String.toList "PLACE") (String.toList "GRUNT")
                    |> Expect.equal
                        [ ( 'P', NotInWord )
                        , ( 'L', NotInWord )
                        , ( 'A', NotInWord )
                        , ( 'C', NotInWord )
                        , ( 'E', NotInWord )
                        ]
        , test "some hits" <|
            \_ ->
                getFeedback (String.toList "PLANT") (String.toList "GRUNT")
                    |> Expect.equal
                        [ ( 'P', NotInWord )
                        , ( 'L', NotInWord )
                        , ( 'A', NotInWord )
                        , ( 'N', Correct )
                        , ( 'T', Correct )
                        ]
        , test "multiple uses of a correct letter" <|
            \_ ->
                getFeedback (String.toList "ROVER") (String.toList "GRAVE")
                    |> Expect.equal
                        [ ( 'R', InWord )
                        , ( 'O', NotInWord )
                        , ( 'V', InWord )
                        , ( 'E', InWord )
                        , ( 'R', NotInWord )
                        ]
        , test "multiple uses of a correct letter, again" <|
            \_ ->
                getFeedback (String.toList "REVOR") (String.toList "EVARG")
                    |> Expect.equal
                        [ ( 'R', InWord )
                        , ( 'E', InWord )
                        , ( 'V', InWord )
                        , ( 'O', NotInWord )
                        , ( 'R', NotInWord )
                        ]
        ]
