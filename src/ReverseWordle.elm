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


type alias Word =
    String


type CharFeedback
    = NotInWord
    | Incorrect
    | InWord
    | Correct


type alias Feedback =
    Dict Int CharFeedback


type Guess
    = Guess Word Feedback
    | NoGuess
    | Solution Word


type alias Guesses =
    Array ( Guess, Feedback )


type WordInput
    = WordInput String
    | RejectedInput String Feedback


type alias SelectionIndex =
    Int


type GameStatus
    = Active SelectionIndex
    | Solved


type alias Model =
    { word : Word
    , guesses : Guesses
    , guessInput : WordInput
    , gameStatus : GameStatus
    }


init : Model
init =
    let
        initWord : Word
        initWord =
            "plane"

        initGuesses : Guesses
        initGuesses =
            List.map (\guess -> getFeedback guess initWord) [ "grams", "spade", "place" ]
                |> List.map (Tuple.pair NoGuess)
                |> Array.fromList
    in
    { word = initWord
    , guesses = initGuesses
    , guessInput = WordInput ""
    , gameStatus = Active (Array.length initGuesses - 1)
    }


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
    = GotWord String
    | WordInputChanged String
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotWord wordInput ->
            case model.gameStatus of
                Solved ->
                    -- shouldn't really be able to get a guess while solved
                    model

                Active index ->
                    let
                        guessFeedback : Feedback
                        guessFeedback =
                            getFeedback wordInput model.word

                        targetFeedback : Feedback
                        targetFeedback =
                            case Array.get index model.guesses of
                                Just ( _, feedback ) ->
                                    feedback

                                Nothing ->
                                    -- this could only happen if the selection index gets messed up
                                    Dict.empty
                    in
                    if simplifyFeedback guessFeedback /= simplifyFeedback targetFeedback then
                        { model | guessInput = RejectedInput wordInput guessFeedback }

                    else
                        let
                            nextGuesses : Guesses
                            nextGuesses =
                                updateGuesses
                                    wordInput
                                    (getFeedback wordInput model.word)
                                    index
                                    model.guesses

                            nextGameStatus : GameStatus
                            nextGameStatus =
                                if isSolved nextGuesses then
                                    Solved

                                else
                                    Active (getNextIndex index nextGuesses)
                        in
                        { model
                            | guesses = nextGuesses
                            , guessInput = WordInput ""
                            , gameStatus = nextGameStatus
                        }

        WordInputChanged guessText ->
            let
                cleanInput : String -> String
                cleanInput str =
                    str |> String.toLower |> String.filter Char.isAlpha
            in
            { model | guessInput = WordInput (cleanInput guessText) }

        ClickedGuess i ->
            case model.gameStatus of
                Active _ ->
                    { model | gameStatus = Active i }

                Solved ->
                    model

        ClickedReset ->
            init


isSolved : Guesses -> Bool
isSolved guesses =
    let
        isNoGuess : ( Guess, Feedback ) -> Bool
        isNoGuess ( guess, _ ) =
            case guess of
                NoGuess ->
                    True

                _ ->
                    False
    in
    Array.isEmpty (Array.filter isNoGuess guesses)


getNextIndex : Int -> Guesses -> Int
getNextIndex currentIndex guesses =
    if isSolved guesses then
        Array.length guesses - 1

    else
        case Array.get currentIndex guesses of
            Just ( NoGuess, _ ) ->
                currentIndex

            Just ( _, _ ) ->
                getNextIndex (currentIndex - 1) guesses

            Nothing ->
                getNextIndex (Array.length guesses - 1) guesses


updateGuesses : Word -> Feedback -> Int -> Guesses -> Guesses
updateGuesses guess feedback index guesses =
    Array.get index guesses
        |> Maybe.withDefault ( NoGuess, Dict.empty )
        |> (\( _, existingFeedback ) ->
                Array.set index ( Guess guess feedback, existingFeedback ) guesses
           )



-- VIEW


solutionFeedback : Feedback
solutionFeedback =
    List.repeat 5 Correct
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


view : Model -> Html Msg
view model =
    let
        getIsSelected : Int -> Bool
        getIsSelected index =
            case model.gameStatus of
                Active selectedIndex ->
                    index == selectedIndex

                Solved ->
                    False

        guessList : List ( Guess, Feedback )
        guessList =
            Array.toList (Array.push ( Solution model.word, solutionFeedback ) model.guesses)
    in
    div [ style "font-size" "20px" ]
        [ h1 [] [ text "Reverse Wordle" ]
        , div [ style "width" "fit-content" ]
            (List.indexedMap (\i ( guess, feedback ) -> viewGuess (getIsSelected i) i guess feedback) guessList)
        , button [ onClick ClickedReset ] [ text "reset" ]
        , if model.gameStatus == Solved then
            h2 [] [ text "you did it!" ]

          else
            viewWordInput model
        ]


formatFeedback : Word -> Dict Int CharFeedback -> List ( CharFeedback, Char )
formatFeedback guess feedback =
    String.toList guess
        |> List.map2 Tuple.pair (Dict.values feedback)


viewGuess : Bool -> Int -> Guess -> Feedback -> Html Msg
viewGuess isSelected index guess feedback =
    case guess of
        Guess word _ ->
            div
                (if isSelected then
                    [ style "border" "1px solid black" ]

                 else
                    [ style "border" "1px solid transparent", onClick (ClickedGuess index) ]
                )
                (List.map viewChar (formatFeedback word feedback))

        Solution word ->
            div [] (List.map viewChar (List.map2 Tuple.pair (List.repeat 5 Correct) (String.toList word)))

        NoGuess ->
            div
                (if isSelected then
                    [ style "border" "1px solid black" ]

                 else
                    [ style "border" "1px solid transparent", onClick (ClickedGuess index) ]
                )
                (List.map viewChar (formatFeedback "     " feedback))


viewChar : ( CharFeedback, Char ) -> Html msg
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


viewWordInput : Model -> Html Msg
viewWordInput model =
    let
        viewInput : String -> Html Msg
        viewInput wordInput =
            form
                [ onSubmit (GotWord wordInput) ]
                [ input [ type_ "text", value wordInput, onInput WordInputChanged, maxlength 5, minlength 5 ] []
                ]
    in
    case model.guessInput of
        WordInput wordInput ->
            div [] [ viewInput wordInput ]

        RejectedInput wordInput feedback ->
            div []
                [ viewInput wordInput
                , text "This guess could not be correct. Your guess would look like this :"
                , div [] (List.map viewChar (formatFeedback wordInput feedback))
                ]
