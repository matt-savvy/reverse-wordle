module ReverseWordle exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, form, h1, h2, input, span, text, ul)
import Html.Attributes exposing (maxlength, minlength, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)



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
    | Solution Word


type alias GuessList =
    Array Guess


type GuessInput
    = GuessInput String
    | RejectedInput String Feedback


type GameStatus
    = Active
    | Solved


type alias Model =
    { word : Word
    , guesses : GuessList
    , guessInput : GuessInput
    , currentGuess : Int
    , gameStatus : GameStatus
    }


init : Model
init =
    let
        initWord : Word
        initWord =
            "plane"

        initGuesses : GuessList
        initGuesses =
            List.map (\guess -> NoGuess (getFeedback guess initWord)) [ "grams", "spade", "place" ]
                |> Array.fromList
    in
    { word = initWord
    , guesses = Array.push (Solution initWord) initGuesses
    , guessInput = GuessInput ""
    , currentGuess = Array.length initGuesses - 1
    , gameStatus = Active
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
    | ClickedGuess Int
    | ClickedReset


simplifyFeedback : Feedback -> Feedback
simplifyFeedback feedback =
    Dict.map
        (\_ charFeedback ->
            case charFeedback of
                Incorrect ->
                    Incorrect

                NotInWord ->
                    Incorrect

                _ ->
                    charFeedback
        )
        feedback
        |> Debug.log "f"


guessInputToString : GuessInput -> String
guessInputToString guessInput =
    case guessInput of
        GuessInput str ->
            str

        RejectedInput str _ ->
            str


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotGuess ->
            let
                guessFeedback : Feedback
                guessFeedback =
                    getFeedback (guessInputToString model.guessInput) model.word

                currentGuess : Maybe Guess
                currentGuess =
                    Array.get model.currentGuess model.guesses

                currentGuessFeedback : Feedback
                currentGuessFeedback =
                    case currentGuess of
                        Just (NoGuess feedback) ->
                            feedback

                        Just (Guess word feedback) ->
                            feedback

                        Just (Solution word) ->
                            getFeedback word word

                        Nothing ->
                            Dict.empty
            in
            case simplifyFeedback guessFeedback == simplifyFeedback currentGuessFeedback of
                True ->
                    let
                        nextGuessList : GuessList
                        nextGuessList =
                            updateGuessList (guessInputToString model.guessInput) (getFeedback (guessInputToString model.guessInput) model.word) model.currentGuess model.guesses

                        nextGameStatus : GameStatus
                        nextGameStatus =
                            if
                                (Array.filter
                                    (\guess ->
                                        case guess of
                                            NoGuess _ ->
                                                True

                                            Guess _ _ ->
                                                False

                                            Solution _ ->
                                                False
                                    )
                                    nextGuessList
                                    |> Array.length
                                )
                                    == 0
                            then
                                Solved

                            else
                                Active
                    in
                    { model
                        | guesses = nextGuessList
                        , guessInput = GuessInput ""
                        , currentGuess = model.currentGuess - 1
                        , gameStatus = nextGameStatus
                    }

                False ->
                    { model | guessInput = RejectedInput (guessInputToString model.guessInput) guessFeedback }

        GuessInputChanged guessText ->
            { model | guessInput = GuessInput (guessText |> String.toLower |> String.filter Char.isAlpha) }

        ClickedGuess i ->
            { model | currentGuess = i }

        ClickedReset ->
            init


updateGuessList : Word -> Feedback -> Int -> GuessList -> GuessList
updateGuessList guess feedback i guesses =
    Array.set i (Guess guess feedback) guesses



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "font-size" "20px" ]
        [ h1 [] [ text "Reverse Wordle" ]
        , div [ style "width" "fit-content" ] (List.map (\( i, guess ) -> viewGuess ((i == model.currentGuess) && (model.gameStatus /= Solved)) i guess) (Array.toIndexedList model.guesses))
        , button [ onClick ClickedReset ] [ text "reset" ]
        , if model.gameStatus == Solved then
            h2 [] [ text "you did it!" ]

          else
            viewGuessInput model
        ]


formatFeedback : Word -> Dict Int CharFeedback -> List ( CharFeedback, Char )
formatFeedback guess feedback =
    String.toList guess
        |> List.map2 Tuple.pair (Dict.values feedback)


viewGuess : Bool -> Int -> Guess -> Html Msg
viewGuess isCurrentGuess index guess =
    case guess of
        Guess word feedback ->
            div
                (if isCurrentGuess then
                    [ style "border" "1px solid black" ]

                 else
                    [ onClick (ClickedGuess index) ]
                )
                (List.map viewChar (formatFeedback word feedback))

        Solution word ->
            div [] (List.map viewChar (List.map2 Tuple.pair (List.repeat 5 Correct) (String.toList word)))

        NoGuess feedback ->
            div
                (if isCurrentGuess then
                    [ style "border" "1px solid black" ]

                 else
                    [ onClick (ClickedGuess index) ]
                )
                (List.map viewChar (formatFeedback "     " feedback))


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
    case model.guessInput of
        GuessInput guessInput ->
            form
                [ onSubmit GotGuess ]
                [ input
                    [ type_ "text", value guessInput, onInput GuessInputChanged, maxlength 5, minlength 5 ]
                    []
                ]

        RejectedInput guessInput _ ->
            form
                [ onSubmit GotGuess ]
                [ input
                    [ type_ "text", value guessInput, onInput GuessInputChanged, maxlength 5, minlength 5 ]
                    []
                , div []
                    [ text "This guess could not be correct. Your guess would look like this :"
                    , div [] (List.map viewChar (formatFeedback guessInput (getFeedback guessInput model.word)))
                    ]
                ]
