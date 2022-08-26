module ReverseWordle exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (Html, button, div, form, h1, h2, h3, input, span, text, toUnstyled)
import Html.Styled.Attributes as Attr exposing (css, id, maxlength, minlength, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Random
import Task
import Words exposing (masterList)



-- MAIN


main : Program Int Model Msg
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


type alias Model =
    { word : Word
    , guesses : Guesses
    , guessInput : WordInput
    , gameStatus : GameStatus
    }


resetGuesses : Guesses -> Guesses
resetGuesses guesses =
    Array.map
        (\( guess, feedback ) ->
            case guess of
                Guess _ _ ->
                    ( NoGuess, feedback )

                NoGuess ->
                    ( NoGuess, feedback )

                Solution _ ->
                    ( NoGuess, solutionFeedback )
        )
        guesses


nearestDay : Int -> Int
nearestDay timestamp =
    let
        -- 1000ms * 60s * 60m * 24h
        day =
            1000 * 60 * 60 * 24
    in
    timestamp - modBy day timestamp


randomWord : Random.Seed -> ( Word, Random.Seed )
randomWord seed =
    let
        wordGenerator =
            Random.uniform "plane" masterList
    in
    Random.step wordGenerator seed


init : Int -> ( Model, Cmd Msg )
init timestamp =
    let
        seed =
            Random.initialSeed (nearestDay timestamp)

        ( word, nextSeed ) =
            randomWord seed

        puzzle =
            createPuzzle word

        guesses =
            solve puzzle nextSeed
                |> Array.map
                    (\entry ->
                        case entry of
                            Guess _ feedback ->
                                -- ( Guess w feedback, feedback )
                                ( NoGuess, feedback )

                            NoGuess ->
                                ( NoGuess, Dict.empty )

                            Solution _ ->
                                ( NoGuess, solutionFeedback )
                    )
                |> Array.filter (\( _, feedback ) -> feedback /= solutionFeedback)
    in
    ( { word = word
      , guesses = guesses
      , guessInput = WordInput ""
      , gameStatus = Active (Array.length guesses - 1)
      }
    , Cmd.none
    )


getFeedback : Word -> Word -> Dict Int CharFeedback
getFeedback guess word =
    getFeedbackFilter (guess |> String.toList |> Array.fromList) (word |> String.toList |> Array.fromList)
        |> addWordDict
        |> getFeedbackHelper
        |> .feedback


addWordDict : FeedbackRecord -> FeedbackRecord
addWordDict { guess, word, feedback } =
    FeedbackRecord guess word (createWordDict (word |> Array.toList |> String.fromList)) feedback


getFeedbackHelper : FeedbackRecord -> FeedbackRecord
getFeedbackHelper feedbackRecord =
    List.foldl
        (\i { guess, word, wordDict, feedback } ->
            if Dict.member i feedback then
                FeedbackRecord guess word wordDict feedback

            else
                case ( Array.get i guess, Array.get i word ) of
                    ( Just guessChar, Just _ ) ->
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
    | ClickedEnter
    | NoOp


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


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "guess-input")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClickedEnter ->
            case model.guessInput of
                WordInput word ->
                    update (GotWord word) model

                _ ->
                    ( model, Cmd.none )

        GotWord wordInput ->
            case model.gameStatus of
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

                appendInput : String -> String
                appendInput str =
                    case model.guessInput of
                        WordInput input ->
                            input ++ str

                        _ ->
                            str
            in
            ( { model | guessInput = WordInput (appendInput (cleanInput guessText)) }, Cmd.none )

        ClickedGuess i _ ->
            case model.gameStatus of
                Active _ ->
                    ( { model | gameStatus = Active i, guessInput = WordInput (getGuessInputByIndex i model.guesses) }, focusInput )

                Solved ->
                    ( model, Cmd.none )

        ClickedReset ->
            let
                -- fresh playable state for this word
                resetGame : ( Model, Cmd Msg )
                resetGame =
                    ( { model
                        | gameStatus = Active (Array.length model.guesses - 1)
                        , guesses = resetGuesses model.guesses
                        , guessInput = WordInput ""
                      }
                    , focusInput
                    )
            in
            case model.gameStatus of
                Active _ ->
                    resetGame

                Solved ->
                    resetGame


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


getGuessInputByIndex : Int -> Guesses -> String
getGuessInputByIndex index guesses =
    Array.get index guesses
        |> Maybe.withDefault ( NoGuess, Dict.empty )
        |> (\( guess, _ ) ->
                case guess of
                    Guess word _ ->
                        word

                    _ ->
                        ""
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


solve : Puzzle -> Random.Seed -> Array Guess
solve (Puzzle eval) seed =
    solveHelper eval masterList seed Array.empty


type alias PossibleWords =
    List Word



-- is the game solved in forwards mode?


isSolvedClassic : Array Guess -> Bool
isSolvedClassic guesses =
    case Array.get (Array.length guesses - 1) guesses of
        Just (Guess _ feedback) ->
            feedback == solutionFeedback

        _ ->
            False


solveHelper : (Word -> Feedback) -> PossibleWords -> Random.Seed -> Array Guess -> Array Guess
solveHelper eval wordList seed guesses =
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
            firstWord :: remainingWordList ->
                let
                    ( guess, nextSeed ) =
                        Random.step (Random.uniform firstWord remainingWordList) seed

                    feedback : Feedback
                    feedback =
                        eval guess
                in
                solveHelper eval (filterWords feedback guess remainingWordList) nextSeed (Array.push (Guess guess feedback) guesses)

            [] ->
                -- if we got here, it means our solver ran out of valid words to try
                -- start solving again from scratch
                solveHelper eval masterList seed Array.empty



-- VIEW


theme :
    { colors :
        { inWord : Color
        , incorrect : Color
        , correct : Color
        , keyColor : Color
        , backgroundColor : Color
        , color : Color
        }
    }
theme =
    { colors =
        { inWord = rgb 181 159 58
        , incorrect = rgb 58 58 60
        , keyColor = rgb 129 131 132
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
        , displayFlex
        , flexDirection column
        , alignItems center
        ]


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
    div [ css [ mainStyle, fontSize (px 16) ] ]
        [ h1 [ css [ opacity (Css.num 0.9) ] ] [ text "Reverse Wordle" ]
        , div []
            (List.indexedMap (\i ( guess, feedback ) -> viewGuess (getIsSelected i) model.guessInput i guess feedback model.gameStatus) guessList)
        , button [ onClick ClickedReset ] [ text "reset" ]
        , case model.gameStatus of
            Solved ->
                h2 [] [ text "you did it!" ]

            Active _ ->
                viewWordInput model
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


getPreview : WordInput -> Word -> String
getPreview wordInput word =
    case wordInput of
        WordInput inputWord ->
            if String.length inputWord > 0 then
                String.padRight 5 ' ' inputWord

            else
                word

        _ ->
            word


viewGuess : Bool -> WordInput -> Int -> Guess -> Feedback -> GameStatus -> Html Msg
viewGuess isSelected guessInput index guess feedback gameStatus =
    let
        viewG : Word -> Html Msg
        viewG word =
            if isSelected then
                div [ css [ selectedGuessStyle ] ] (List.indexedMap (viewChar index) (formatFeedback (getPreview guessInput word) feedback))

            else
                div [ css [ guessStyle ] ] (List.indexedMap (viewChar index) (formatFeedback word feedback))
    in
    case guess of
        Guess word _ ->
            div [] [ viewG word ]

        NoGuess ->
            case gameStatus of
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


viewInput : String -> Html Msg
viewInput wordInput =
    form
        [ onSubmit (GotWord wordInput), css [ displayFlex, flexDirection column, alignItems center ] ]
        [ input [ id "guess-input", type_ "text", value wordInput, Attr.required True, onInput WordInputChanged, maxlength 5, minlength 5 ] []
        ]


viewKeyboard : Html Msg
viewKeyboard =
    let
        viewKeys : String -> List (Html Msg)
        viewKeys letters =
            String.split "" letters
                |> List.map (\letter -> viewKey letter (WordInputChanged letter))

        viewKey : String -> Msg -> Html Msg
        viewKey label handler =
            button
                [ css
                    [ backgroundColor theme.colors.keyColor
                    , margin2 (px 2) (px 6)
                    , color theme.colors.color
                    , height (px 48)
                    , minWidth (px 33)
                    ]
                , onClick handler
                ]
                [ text (String.toUpper label) ]

        topRow =
            "qwertyuiop"

        homeRow =
            "asdfghjkl"

        bottomRow =
            "zxcvbnm"
    in
    div []
        [ div [] (viewKeys topRow)
        , div [] (viewKeys homeRow)
        , div [] (viewKey "ENTER" ClickedEnter :: viewKeys bottomRow)
        ]


viewWordInput : Model -> Html Msg
viewWordInput model =
    case model.guessInput of
        WordInput _ ->
            div [] [ viewKeyboard ]

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
            div [ css [ displayFlex, flexDirection column, alignItems center ] ]
                [ viewKeyboard
                , h3 [] [ text "This guess could not be correct. " ]
                , text "Your guess would look like this:"
                , div [ css [ guessStyle ] ] (List.indexedMap (viewChar 0) (formatFeedback wordInput feedback))
                , text "But it needs to look like this: "
                , div [ css [ guessStyle ] ] (List.indexedMap (viewChar 0) (formatFeedback "     " targetFeedback))
                ]

        NotAWord wordInput ->
            div []
                [ viewKeyboard
                , text (wordInput ++ " is not a word in our dictionary")
                ]
