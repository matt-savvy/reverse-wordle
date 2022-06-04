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
    { word : Word }


init : Model
init =
    { word = [ 'H', 'E', 'L', 'L', 'O' ] }


getFeedback : Char -> Word -> Feedback
getFeedback char word =
    case char of
        'H' ->
            InWord

        'O' ->
            InWord

        'E' ->
            Correct

        _ ->
            NotInWord


type alias Word =
    List Char


type Feedback
    = NotInWord
    | InWord
    | Correct



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Model
update _ model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ viewGuess model.word ]


viewGuess : Word -> Html Msg
viewGuess word =
    div [] (List.map (\char -> viewChar (getFeedback char word) char) word)


viewChar : Feedback -> Char -> Html Msg
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
