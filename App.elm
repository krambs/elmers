module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { answer : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "¯\\_(ツ)_/¯"
    , getYesOrNoAnswer
    )



-- UPDATE


type Msg
    = MorePlease
    | FetchSucceed String
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getYesOrNoAnswer )

        FetchSucceed answer ->
            ( Model answer, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.answer ]
        , button [ onClick MorePlease ] [ text "Yes or no?" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getYesOrNoAnswer : Cmd Msg
getYesOrNoAnswer =
    let
        url =
            "http://yesno.wtf/api"
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeAnswer url)


decodeAnswer : Json.Decoder String
decodeAnswer =
    Json.at [ "answer" ] Json.string
