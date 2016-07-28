module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
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
    { query : String
    , jobs : List Job
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" []
    , submitSearch ""
    )


type alias Job =
    { id : String
    , position_title : String
    , organization_name : String
    , rate_interval_code : String
    , minimum : Int
    , maximum : Int
    , start_date : String
    , end_date : String
    , locations : List String
    , url : String
    }



-- UPDATE


type Msg
    = Query String
    | SubmitSearch
    | FetchSucceed (List Job)
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Query query ->
            { model | query = query } ! []

        SubmitSearch ->
            ( model, submitSearch model.query )

        FetchSucceed answer ->
            ( Model model.query answer, Cmd.none )

        FetchFail error ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Query, onSubmit SubmitSearch ] []
        , button [ onClick SubmitSearch ] [ text "Search Jobs" ]
        , jobEntries model.jobs
        ]


jobEntries : List Job -> Html Msg
jobEntries jobs =
    div []
        (List.map jobEntry jobs)


jobEntry : Job -> Html Msg
jobEntry job =
    div []
        [ h2 [] [ text job.position_title ]
        , div [] [ text ("Id: " ++ job.id) ]
        , div [] [ text ("Organization: " ++ job.organization_name) ]
        , div [] [ text ("Rate Interval Code: " ++ job.rate_interval_code) ]
        , div [] [ text ("Minimum: " ++ (toString job.minimum)) ]
        , div [] [ text ("Maximum: " ++ (toString job.maximum)) ]
        , div [] [ text ("Start Date: " ++ job.start_date) ]
        , div [] [ text ("End Date: " ++ job.end_date) ]
        , div [] [ text ("URL: " ++ job.url) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


submitSearch : String -> Cmd Msg
submitSearch query =
    let
        url =
            "https://api.usa.gov/jobs/search.json?query=" ++ query
    in
        Task.perform FetchFail FetchSucceed (Http.get decodeJobs url)


decodeJobs : Decoder (List Job)
decodeJobs =
    list decodeJob


decodeJob : Decoder Job
decodeJob =
    succeed Job
        `apply` ("id" := string)
        `apply` ("position_title" := string)
        `apply` ("organization_name" := string)
        `apply` ("rate_interval_code" := string)
        `apply` ("minimum" := int)
        `apply` ("maximum" := int)
        `apply` ("start_date" := string)
        `apply` ("end_date" := string)
        `apply` ("locations" := list string)
        `apply` ("url" := string)



-- UTILITIES
-- Applicative's `<*>`:
-- TODO replace with elm-json-extra when that is compatible with Elm 0.17


apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply =
    object2 (<|)
