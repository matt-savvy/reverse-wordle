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
    { word = [ 'P', 'L', 'A', 'N', 'E' ]
    , guesses =
        [ [ 'P', 'E', 'A', 'C', 'E' ]
        ]
    }


getFeedback : Word -> Word -> Feedback
getFeedback guess word =
    getFeedbackHelper guess word []


getFeedbackHelper : Word -> Word -> Feedback -> Feedback
getFeedbackHelper guess word accumulatedFeedback =
    case ( guess, word ) of
        ( [], [] ) ->
            accumulatedFeedback

        ( char :: remainingGuess, wordChar :: remainingWord ) ->
            let
                charFeedback : CharFeedback
                charFeedback =
                    if char == wordChar then
                        Correct

                    else if List.member char remainingWord then
                        InWord

                    else
                        NotInWord
            in
            getFeedbackHelper remainingGuess remainingWord (accumulatedFeedback ++ [ ( char, charFeedback ) ])

        ( _, _ ) ->
            accumulatedFeedback


type alias Word =
    List Char


type CharFeedback
    = NotInWord
    | InWord
    | Correct


type alias Feedback =
    List ( Char, CharFeedback )



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
    div [] (List.map (\( char, charFeedback ) -> viewChar charFeedback char) (getFeedback guess model.word))


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
