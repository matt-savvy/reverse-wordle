module ReverseWordle exposing (..)

import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { word : Word
    , guesses : List Word
    }


init : Model
init =
    { word = [ 'P', 'L', 'A', 'C', 'E' ]
    , guesses =
        [ [ 'P', 'E', 'A', 'C', 'E' ]
        ]
    }


getFeedback : Word -> Word -> Feedback
getFeedback guess word =
    [ InWord
    , Correct
    , InWord
    , NotInWord
    , Correct
    ]


type alias Word =
    List Char


type CharFeedback
    = NotInWord
    | InWord
    | Correct


type alias Feedback =
    List CharFeedback



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Model
update _ model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div [] (List.map (viewGuess model) model.guesses)


viewGuess : Model -> Word -> Html Msg
viewGuess model guess =
    let
        guessWithFeedback : List ( Char, CharFeedback )
        guessWithFeedback =
            List.map2 (\a b -> ( a, b )) guess (getFeedback guess model.word)
    in
    div [] (List.map (\( char, charFeedback ) -> viewChar charFeedback char) guessWithFeedback)


viewChar : CharFeedback -> Char -> Html Msg
viewChar feedback char =
    let
        feedbackColor : String
        feedbackColor =
            case feedback of
                NotInWord ->
                    "gray"

                InWord ->
                    "yellow"

                Correct ->
                    "green"
    in
    span
        [ style "padding" "2px 4px"
        , style "margin" "2px 4px"
        , style "background-color" feedbackColor
        ]
        [ text (String.fromChar char) ]
