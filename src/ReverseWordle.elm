module ReverseWordle exposing (..)

import Array exposing (Array)
import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (Html, button, div, form, h1, h2, input, label, span, text, toUnstyled)
import Html.Styled.Attributes as Attr exposing (css, disabled, maxlength, minlength, required, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Random
import Words exposing (masterList)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = \_ -> Sub.none
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
    | NotAWord String


type alias SelectionIndex =
    Int


type GameStatus
    = Active SelectionIndex
    | Solved
    | SetupWord
    | SetupGuesses
    | GeneratePuzzle


type alias Model =
    { word : Word
    , guesses : Guesses
    , guessInput : WordInput
    , gameStatus : GameStatus
    }


initGame : Word -> Model
initGame word =
    let
        puzzle =
            createPuzzle word

        guesses =
            solve puzzle
                |> Debug.log "puzzle"
                |> Array.map
                    (\entry ->
                        case entry of
                            Guess w feedback ->
                                -- ( Guess w feedback, feedback )
                                ( NoGuess, feedback )

                            NoGuess ->
                                ( NoGuess, Dict.empty )

                            Solution _ ->
                                ( NoGuess, solutionFeedback )
                    )
                |> Array.filter (\( _, feedback ) -> feedback /= solutionFeedback)
    in
    { word = word
    , guesses = guesses
    , guessInput = WordInput ""
    , gameStatus = Active (Array.length guesses - 1)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { word = ""
      , guesses = Array.empty
      , guessInput = WordInput ""
      , gameStatus = GeneratePuzzle
      }
    , Random.generate GotWord (Random.uniform "plane" masterList)
    )


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
    | ClickedGuess Int Int
    | ClickedReset
    | ClickedAddGuess
    | ClickedRemoveGuess SelectionIndex
    | ClickedFinishedSetup


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


getTargetFeedback : SelectionIndex -> Guesses -> Feedback
getTargetFeedback index guesses =
    case Array.get index guesses of
        Just ( _, feedback ) ->
            feedback

        Nothing ->
            -- this could only happen if the selection index gets messed up
            Dict.empty


getWord : Int -> String
getWord i =
    let
        doesntContain : String -> String -> Bool
        doesntContain substr str =
            not (String.contains substr str)
    in
    Array.fromList masterList
        |> Array.filter (doesntContain "ight")
        |> Array.get i
        |> Maybe.withDefault "plane"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWord wordInput ->
            case model.gameStatus of
                GeneratePuzzle ->
                    ( initGame wordInput, Cmd.none )

                SetupWord ->
                    ( { model | word = wordInput, gameStatus = SetupGuesses, guessInput = WordInput "" }, Cmd.none )

                SetupGuesses ->
                    -- shouldn't really be able to get a guess while solved
                    ( model, Cmd.none )

                Solved ->
                    -- shouldn't really be able to get a guess while solved
                    ( model, Cmd.none )

                Active index ->
                    let
                        guessFeedback : Feedback
                        guessFeedback =
                            getFeedback wordInput model.word

                        targetFeedback : Feedback
                        targetFeedback =
                            getTargetFeedback index model.guesses
                    in
                    if not (List.member wordInput masterList) then
                        ( { model | guessInput = NotAWord wordInput }, Cmd.none )

                    else if simplifyFeedback guessFeedback /= simplifyFeedback targetFeedback then
                        ( { model | guessInput = RejectedInput wordInput guessFeedback }, Cmd.none )

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
                        ( { model | guesses = nextGuesses, guessInput = WordInput "", gameStatus = nextGameStatus }, Cmd.none )

        WordInputChanged guessText ->
            let
                cleanInput : String -> String
                cleanInput str =
                    str |> String.toLower |> String.filter Char.isAlpha
            in
            ( { model | guessInput = WordInput (cleanInput guessText) }, Cmd.none )

        ClickedGuess i j ->
            case model.gameStatus of
                SetupWord ->
                    ( model, Cmd.none )

                SetupGuesses ->
                    ( { model | guesses = updateFeedback i j model.guesses }, Cmd.none )

                Active _ ->
                    ( { model | gameStatus = Active i }, Cmd.none )

                Solved ->
                    ( model, Cmd.none )

                GeneratePuzzle ->
                    ( model, Cmd.none )

        ClickedAddGuess ->
            ( { model | guesses = Array.slice 0 5 (Array.push ( NoGuess, initFeedback ) model.guesses) }, Cmd.none )

        ClickedRemoveGuess i ->
            ( { model | guesses = removeAtIndex i model.guesses }, Cmd.none )

        ClickedFinishedSetup ->
            ( { model | gameStatus = Active (Array.length model.guesses - 1) }, Cmd.none )

        ClickedReset ->
            -- TODO handle what mode we're in
            init ()


removeAtIndex : Int -> Array a -> Array a
removeAtIndex index arr =
    Array.append
        (Array.slice 0 index arr)
        (Array.slice (index + 1) (Array.length arr) arr)


isSolved : Guesses -> Bool
isSolved guesses =
    if Array.isEmpty guesses then
        False

    else
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


updateFeedback : SelectionIndex -> Int -> Guesses -> Guesses
updateFeedback index charIndex guesses =
    let
        cycleFeedback : Feedback -> Feedback
        cycleFeedback feedback =
            Dict.update charIndex
                (\maybeFeedbackChar ->
                    case maybeFeedbackChar of
                        Just Incorrect ->
                            Just InWord

                        Just InWord ->
                            Just Correct

                        _ ->
                            Just Incorrect
                )
                feedback
    in
    Array.get index guesses
        |> Maybe.withDefault ( NoGuess, initFeedback )
        |> (\( guess, feedback ) ->
                Array.set index ( guess, cycleFeedback feedback ) guesses
           )



-- SOLVER


type Puzzle
    = Puzzle (Word -> Feedback)


type PuzzleResult
    = Success Guesses
    | Failure


createPuzzle : Word -> Puzzle
createPuzzle word =
    Puzzle (\guess -> getFeedback guess word)


solve : Puzzle -> Array Guess
solve (Puzzle eval) =
    solveHelper eval masterList Array.empty


type alias PossibleWords =
    List Word



-- is the game solved in forwards mode?


isSolvedClassic : Array Guess -> Bool
isSolvedClassic guesses =
    case Array.get (Array.length guesses - 1) guesses of
        Just (Guess word feedback) ->
            feedback == solutionFeedback

        _ ->
            False


solveHelper : (Word -> Feedback) -> PossibleWords -> Array Guess -> Array Guess
solveHelper eval wordList guesses =
    let
        filterWords : Feedback -> Word -> PossibleWords -> PossibleWords
        filterWords feedback lastGuess remainingWords =
            List.filter (\possibleWord -> getFeedback lastGuess possibleWord == feedback) remainingWords
    in
    if isSolvedClassic guesses then
        guesses

    else if Array.length guesses > 6 then
        guesses

    else
        case wordList of
            guess :: remainingWordList ->
                let
                    feedback : Feedback
                    feedback =
                        eval guess
                in
                solveHelper eval (filterWords feedback guess remainingWordList) (Array.push (Guess guess feedback) guesses)

            [] ->
                Debug.todo "replace this with PuzzleResult failure"



-- VIEW


theme =
    { colors =
        { inWord = rgb 181 159 58
        , incorrect = rgb 58 58 60
        , correct = rgb 83 141 78
        , backgroundColor = rgb 18 18 18
        , color = rgb 255 255 255
        }
    }


initFeedback : Feedback
initFeedback =
    List.repeat 5 Incorrect
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


solutionFeedback : Feedback
solutionFeedback =
    List.repeat 5 Correct
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


mainStyle : Style
mainStyle =
    Css.batch
        [ backgroundColor theme.colors.backgroundColor
        , color theme.colors.color
        , fontFamilies [ "Helvetica Neue", "Arial", "sans-serif" ]
        ]


view : Model -> Html Msg
view model =
    let
        getIsSelected : Int -> Bool
        getIsSelected index =
            case model.gameStatus of
                SetupWord ->
                    False

                SetupGuesses ->
                    False

                Active selectedIndex ->
                    index == selectedIndex

                Solved ->
                    False

                GeneratePuzzle ->
                    False

        guessList : List ( Guess, Feedback )
        guessList =
            Array.toList (Array.push ( Solution model.word, solutionFeedback ) model.guesses)
    in
    div [ css [ mainStyle, fontSize (px 16) ] ]
        [ h1 [ css [ opacity (Css.num 0.9) ] ] [ text "Reverse Wordle" ]
        , div [ css [ maxWidth fitContent ] ]
            (List.indexedMap (\i ( guess, feedback ) -> viewGuess (getIsSelected i) i guess feedback model.gameStatus) guessList)
        , button [ onClick ClickedReset ] [ text "reset" ]
        , case model.gameStatus of
            Solved ->
                h2 [] [ text "you did it!" ]

            SetupWord ->
                label [] [ text "enter your word", viewWordInput model ]

            SetupGuesses ->
                div []
                    [ button [ Attr.disabled (Array.length model.guesses >= 5), onClick ClickedAddGuess ] [ text "add guess" ]

                    --  TODO disable this if the feedback is empty ?
                    , button [ onClick ClickedFinishedSetup ] [ text "finished setup" ]
                    ]

            Active _ ->
                viewWordInput model

            GeneratePuzzle ->
                text ""
        ]


formatFeedback : Word -> Dict Int CharFeedback -> List ( CharFeedback, Char )
formatFeedback guess feedback =
    String.toList guess
        |> List.map2 Tuple.pair (Dict.values feedback)


guessStyle : Style
guessStyle =
    Css.batch
        [ border3 (px 1) solid transparent
        , displayFlex
        , justifyContent spaceAround
        , width (px 200)
        , padding (px 2)
        , margin2 (px 2) (px 2)
        ]


selectedGuessStyle : Style
selectedGuessStyle =
    Css.batch [ guessStyle, border3 (px 1) solid (rgb 128 128 128) ]


viewGuess : Bool -> Int -> Guess -> Feedback -> GameStatus -> Html Msg
viewGuess isSelected index guess feedback gameStatus =
    let
        viewG : Word -> Html Msg
        viewG word =
            div
                (if isSelected then
                    [ css [ selectedGuessStyle ] ]

                 else
                    [ css [ guessStyle ] ]
                )
                (List.indexedMap (viewChar index) (formatFeedback word feedback))
    in
    case guess of
        Guess word _ ->
            div [] [ viewG word ]

        NoGuess ->
            case gameStatus of
                SetupGuesses ->
                    div [] [ viewG "     ", button [ onClick (ClickedRemoveGuess index) ] [ text "x" ] ]

                _ ->
                    div [] [ viewG "     " ]

        Solution word ->
            div [ css [ guessStyle ] ] (List.indexedMap (viewChar index) (formatFeedback word feedback))


viewChar : SelectionIndex -> Int -> ( CharFeedback, Char ) -> Html Msg
viewChar guessIndex charIndex ( feedback, char ) =
    let
        feedbackColor : Color
        feedbackColor =
            case feedback of
                NotInWord ->
                    theme.colors.incorrect

                Incorrect ->
                    theme.colors.incorrect

                InWord ->
                    theme.colors.inWord

                Correct ->
                    theme.colors.correct
    in
    span
        [ css
            [ minHeight (px 33)
            , minWidth (px 33)
            , display inlineFlex
            , alignItems center
            , justifyContent center
            , verticalAlign middle
            , lineHeight (px 16)
            , backgroundColor feedbackColor
            , fontWeight (int 700)
            ]
        , onClick (ClickedGuess guessIndex charIndex)
        ]
        [ text (String.fromChar char |> String.toUpper) ]


viewWordInput : Model -> Html Msg
viewWordInput model =
    let
        viewInput : String -> Html Msg
        viewInput wordInput =
            form
                [ onSubmit (GotWord wordInput) ]
                [ input [ type_ "text", value wordInput, Attr.required True, onInput WordInputChanged, maxlength 5, minlength 5 ] []
                ]
    in
    case model.guessInput of
        WordInput wordInput ->
            div [] [ viewInput wordInput ]

        RejectedInput wordInput feedback ->
            let
                targetFeedback : Feedback
                targetFeedback =
                    case model.gameStatus of
                        Active index ->
                            getTargetFeedback index model.guesses

                        _ ->
                            -- this shouldn't happen either
                            Dict.empty
            in
            div []
                [ viewInput wordInput
                , text "This guess could not be correct. Your guess would look like this:"
                , div [] (List.indexedMap (viewChar 0) (formatFeedback wordInput feedback))
                , text "But it needs to look like this: "
                , div [] (List.indexedMap (viewChar 0) (formatFeedback "     " targetFeedback))
                ]

        NotAWord wordInput ->
            div []
                [ viewInput wordInput
                , text (wordInput ++ " is not a word in our dictionary")
                ]
