module ReverseWordle exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, form, h1, input, span, text, ul)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput, onSubmit)



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
    , guessInput : String
    }


init : Model
init =
    { word = "plane"
    , guesses =
        [ "peace" ]
    , guessInput = ""
    }


type alias Word =
    String


type CharFeedback
    = NotInWord
    | InWord
    | Correct


type alias Feedback =
    Dict Int CharFeedback


getFeedback : Word -> Word -> Dict Int CharFeedback
getFeedback guess word =
    getFeedbackFilter (guess |> String.toList |> Array.fromList) (word |> String.toList |> Array.fromList)
        |> addWordDict
        |> (\feedback -> Debug.log "wordDict added" feedback)
        |> getFeedbackHelper
        |> .feedback


addWordDict : FeedbackRecord -> FeedbackRecord
addWordDict { guess, word, wordDict, feedback } =
    FeedbackRecord guess word (createWordDict (word |> Array.toList |> String.fromList)) feedback


getFeedbackHelper : FeedbackRecord -> FeedbackRecord
getFeedbackHelper feedbackRecord =
    List.foldl
        (\i { guess, word, wordDict, feedback } ->
            if Dict.member i (Debug.log "feedback" feedback) then
                FeedbackRecord guess word wordDict feedback

            else
                case ( Array.get i guess, Array.get i word ) of
                    ( Just guessChar, Just wordChar ) ->
                        if (Dict.get guessChar wordDict |> Maybe.withDefault 0) > 0 then
                            FeedbackRecord guess word (Dict.update guessChar decrementCount wordDict) (Dict.insert i InWord feedback)

                        else
                            FeedbackRecord guess word (Debug.log (String.cons guessChar " not in wordDict") wordDict) (Dict.insert i NotInWord feedback)

                    _ ->
                        FeedbackRecord guess word wordDict feedback
        )
        feedbackRecord
        (List.range 0 4)


type alias FeedbackRecord =
    { guess : Array Char
    , word : Array Char
    , wordDict : Dict Char Int
    , feedback : Dict Int CharFeedback
    }


getFeedbackFilter : Array Char -> Array Char -> FeedbackRecord
getFeedbackFilter guessArray wordArray =
    List.foldl
        (\i { guess, word, wordDict, feedback } ->
            case ( Array.get i guessArray, Array.get i wordArray ) of
                ( Just guessChar, Just wordChar ) ->
                    if guessChar == wordChar then
                        FeedbackRecord (Array.push '!' guess) (Array.push '!' word) wordDict (Dict.insert i Correct feedback)

                    else
                        FeedbackRecord (Array.push guessChar guess) (Array.push wordChar word) wordDict feedback

                _ ->
                    FeedbackRecord guess word wordDict feedback
        )
        (FeedbackRecord Array.empty Array.empty Dict.empty Dict.empty)
        (List.range 0 4)


increrementCount : Maybe Int -> Maybe Int
increrementCount maybeCount =
    case maybeCount of
        Just count ->
            Just (count + 1)

        Nothing ->
            Just 1


decrementCount : Maybe Int -> Maybe Int
decrementCount maybeCount =
    case maybeCount of
        Just count ->
            Just (count - 1)

        Nothing ->
            Just 0


createWordDict : Word -> Dict Char Int
createWordDict word =
    List.foldl
        (\char wordDict -> Dict.update char increrementCount wordDict)
        Dict.empty
        (String.toList word)



-- UPDATE


type Msg
    = GotGuess
    | GuessInputChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotGuess ->
            { model | guesses = model.guesses ++ [ model.guessInput ], guessInput = "" }

        GuessInputChanged guessText ->
            { model | guessInput = guessText }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.word ]
        , ul [] (List.map (viewGuess model) model.guesses)
        , viewGuessInput model
        ]


formatFeedback : Word -> Dict Int CharFeedback -> List ( CharFeedback, Char )
formatFeedback guess feedback =
    String.toList guess
        |> List.map2 Tuple.pair (Dict.values feedback)


viewGuess : Model -> Word -> Html Msg
viewGuess model guess =
    let
        feedback : List ( CharFeedback, Char )
        feedback =
            getFeedback guess model.word |> formatFeedback guess
    in
    div [] (List.map viewChar feedback)


viewChar : ( CharFeedback, Char ) -> Html Msg
viewChar ( feedback, char ) =
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
        [ text (String.fromChar char |> String.toUpper) ]


viewGuessInput : Model -> Html Msg
viewGuessInput model =
    form [ onSubmit GotGuess ] [ input [ type_ "text", value model.guessInput, onInput GuessInputChanged ] [] ]



-- input [type_ "text", onInput GuessInputChanged] []
