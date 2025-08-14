module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text, ul, li)
import Http
import Json.Decode as D

-- MODEL

type alias Exercise =
    { exerciseName : String
    , muscleGroup : String
    , prescribedSets : Int
    , prescribedReps : Maybe Int
    }

exerciseDecoder : D.Decoder Exercise
exerciseDecoder =
    D.map4 Exercise
        (D.field "exerciseName" D.string)
        (D.field "muscleGroup" D.string)
        (D.field "prescribedSets" D.int)
        (D.field "prescribedReps" (D.nullable D.int))


type alias Workout =
    { workoutName : String
    , exercises : List Exercise
    }

workoutDecoder : D.Decoder Workout
workoutDecoder =
    D.map2 Workout
        (D.field "workoutName" D.string)
        (D.field "exercises" (D.list exerciseDecoder))


type alias Week =
    { weekNumber : Int
    , workouts : List Workout
    }

weekDecoder : D.Decoder Week
weekDecoder =
    D.map2 Week
        (D.field "weekNumber" D.int)
        (D.field "workouts" (D.list workoutDecoder))


type alias Plan =
    { numWeeks : Int
    , weeks : List Week
    }

planDecoder : D.Decoder Plan
planDecoder =
    D.map2 Plan
        (D.field "numWeeks" D.int)
        (D.field "weeks" (D.list weekDecoder))


type Model
    = Loading
    | Failure String
    | Loaded Plan


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, fetchPlan )


-- UPDATE

type Msg
    = GotPlan (Result Http.Error Plan)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlan (Ok plan) ->
            ( Loaded plan, Cmd.none )

        GotPlan (Err err) ->
            ( Failure (debugHttp err), Cmd.none )


fetchPlan : Cmd Msg
fetchPlan =
    Http.get
        { url = "/api/plan"
        , expect = Http.expectJson GotPlan planDecoder
        }


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [ h1 [] [ text "Loading plan..." ] ]

        Failure e ->
            div [] [ h1 [] [ text "Error" ], div [] [ text e ] ]

        Loaded plan ->
            div []
                [ h1 [] [ text "Workout Plan" ]
                , ul [] (List.concatMap viewWeek plan.weeks)
                ]

viewWeek : Week -> List (Html msg)
viewWeek week =
    let
        header = li [] [ text ("Week " ++ String.fromInt week.weekNumber) ]
        workouts = List.map viewWorkout week.workouts
    in
    header :: workouts

viewWorkout : Workout -> Html msg
viewWorkout w =
    li []
        [ text w.workoutName
        , ul [] (List.map viewExercise w.exercises)
        ]

viewExercise : Exercise -> Html msg
viewExercise ex =
    li [] [ text (ex.exerciseName ++ " (" ++ ex.muscleGroup ++ ") sets: " ++ String.fromInt ex.prescribedSets) ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

-- UTIL

debugHttp : Http.Error -> String
debugHttp err =
    case err of
        Http.BadUrl u -> "BadUrl: " ++ u
        Http.Timeout -> "Timeout"
        Http.NetworkError -> "NetworkError"
        Http.BadStatus r -> "BadStatus: " ++ String.fromInt r.status.code
        Http.BadBody m -> "BadBody: " ++ m
