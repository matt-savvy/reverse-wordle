module ReverseWordle exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, form, h1, input, span, text, ul)
import Html.Attributes exposing (maxlength, minlength, style, type_, value)
import Html.Events exposing (onInput, onSubmit)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type Guess
    = Guess Word Feedback
    | NoGuess Feedback


type alias GuessList =
    Array Guess


type alias Model =
    { word : Word
    , guesses : GuessList
    , guessInput : String
    }


init : Model
init =
    let
        initWord : Word
        initWord =
            "plane"

        initGuesses : GuessList
        initGuesses =
            List.map (\guess -> NoGuess (getFeedback guess initWord)) [ "grams", "space", "place" ]
                |> Array.fromList
    in
    { word = initWord
    , guesses = Array.push (Guess initWord (getFeedback initWord initWord)) initGuesses
    , guessInput = ""
    }


type alias Word =
    String


type CharFeedback
    = NotInWord
    | Incorrect
    | InWord
    | Correct


type alias Feedback =
    Dict Int CharFeedback


getFeedback : Word -> Word -> Dict Int CharFeedback
getFeedback guess word =
    getFeedbackFilter (guess |> String.toList |> Array.fromList) (word |> String.toList |> Array.fromList)
        |> addWordDict
        |> getFeedbackHelper
        |> .feedback


addWordDict : FeedbackRecord -> FeedbackRecord
addWordDict { guess, word, wordDict, feedback } =
    FeedbackRecord guess word (createWordDict (word |> Array.toList |> String.fromList)) feedback


getFeedbackHelper : FeedbackRecord -> FeedbackRecord
getFeedbackHelper feedbackRecord =
    List.foldl
        (\i { guess, word, wordDict, feedback } ->
            if Dict.member i feedback then
                FeedbackRecord guess word wordDict feedback

            else
                case ( Array.get i guess, Array.get i word ) of
                    ( Just guessChar, Just wordChar ) ->
                        if (Dict.get guessChar wordDict |> Maybe.withDefault 0) > 0 then
                            FeedbackRecord guess word (Dict.update guessChar decrementCount wordDict) (Dict.insert i InWord feedback)

                        else if List.member guessChar (Array.toList word) then
                            FeedbackRecord guess word wordDict (Dict.insert i Incorrect feedback)

                        else
                            FeedbackRecord guess word wordDict (Dict.insert i NotInWord feedback)

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
            { model
                | guesses = updateGuessList model.guessInput (getFeedback model.guessInput model.word) model.guesses
                , guessInput = ""
            }

        GuessInputChanged guessText ->
            { model | guessInput = guessText }


updateGuessList : Word -> Feedback -> GuessList -> GuessList
updateGuessList guess feedback guesses =
    Array.append (Array.fromList [ Guess guess feedback ]) guesses



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "font-size" "20px" ]
        [ div [] (List.map viewGuess (Array.toList model.guesses))
        , viewGuessInput model
        ]


formatFeedback : Word -> Dict Int CharFeedback -> List ( CharFeedback, Char )
formatFeedback guess feedback =
    String.toList guess
        |> List.map2 Tuple.pair (Dict.values feedback)


viewGuess : Guess -> Html Msg
viewGuess guess =
    case guess of
        Guess word feedback ->
            div [] (List.map viewChar (formatFeedback word feedback))

        NoGuess feedback ->
            div [] (List.map viewChar (formatFeedback "     " feedback))


viewChar : ( CharFeedback, Char ) -> Html Msg
viewChar ( feedback, char ) =
    let
        feedbackColor : String
        feedbackColor =
            case feedback of
                NotInWord ->
                    "gray"

                Incorrect ->
                    "gray"

                InWord ->
                    "yellow"

                Correct ->
                    "green"
    in
    span
        [ style "padding" "2px 4px"
        , style "margin" "2px 4px"
        , style "height" "1em"
        , style "min-width" "12px"
        , style "display" "inline-block"
        , style "font-family" "monospace"
        , style "background-color" feedbackColor
        ]
        [ text (String.fromChar char |> String.toUpper) ]


viewGuessInput : Model -> Html Msg
viewGuessInput model =
    form [ onSubmit GotGuess ] [ input [ type_ "text", value model.guessInput, onInput GuessInputChanged, maxlength 5, minlength 5 ] [] ]
