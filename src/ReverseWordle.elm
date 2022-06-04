module ReverseWordle exposing (..)

import Browser
import Html exposing (Html, div, text)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    ()


init : Model
init =
    ()



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Model
update _ model =
    model



-- VIEW


view : Model -> Html Msg
view _ =
    div [] [ text "sanity" ]
