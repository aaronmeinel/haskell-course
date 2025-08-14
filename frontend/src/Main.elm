module Main exposing (main)

import Browser
import Platform.Sub as Sub
import Html exposing (Html, div, h1, text, ul, li, span, nav, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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


-- ROUTING

type Route
    = Overview
    | Current

type Status
    = Loading
    | Failure String
    | Loaded Plan

type alias Model =
    { status : Status
    , route : Route
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Loading, route = Current }
    , fetchPlan
    )


-- UPDATE

type Msg
    = GotPlan (Result Http.Error Plan)
    | SetRoute Route

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlan (Ok plan) ->
            ( { model | status = Loaded plan }, Cmd.none )

        GotPlan (Err err) ->
            ( { model | status = Failure (debugHttp err) }, Cmd.none )

        SetRoute r ->
            ( { model | route = r }, Cmd.none )


fetchPlan : Cmd Msg
fetchPlan =
    Http.get
        { url = "/api/plan"
        , expect = Http.expectJson GotPlan planDecoder
        }


-- VIEW

view : Model -> Html Msg
view model =
    let
        body =
            case model.status of
                Loading ->
                    div [ class "flex items-center gap-3 text-sm text-slate-400 animate-pulse" ]
                        [ loaderIcon, text "Loading plan..." ]

                Failure e ->
                    div [ class "bg-red-950/60 border border-red-500/40 text-red-200 rounded-xl px-5 py-4 text-sm backdrop-blur shadow ring-1 ring-red-500/10" ] [ text e ]

                Loaded plan ->
                    case model.route of
                        Overview ->
                            ul [ class "grid gap-7 md:grid-cols-2 xl:grid-cols-3" ] (List.concatMap viewWeek plan.weeks)

                        Current ->
                            viewCurrent plan
    in
    div []
        [ viewNav model.route
        , div [ class "mt-6" ] [ body ]
        ]

viewNav : Route -> Html Msg
viewNav currentRoute =
    let
        tab r title icon =
            let
                active = r == currentRoute
                base = "flex items-center gap-2 px-4 h-9 rounded-lg text-sm font-medium transition-colors duration-150"
                activeCls = if active then " bg-slate-800/70 text-sky-300 ring-1 ring-sky-400/40" else " text-slate-400 hover:text-slate-200 hover:bg-slate-800/40"
            in
            button
                [ class (base ++ activeCls)
                , onClick (SetRoute r)
                ]
                [ span [ class "text-base" ] [ text icon ]
                , text title
                ]
    in
    nav [ class "flex flex-wrap gap-2" ]
        [ tab Current "Current" "▶"
        , tab Overview "Overview" "☰"
        ]

viewCurrent : Plan -> Html Msg
viewCurrent plan =
    case firstWorkout plan of
        Nothing ->
            div [ class "text-slate-400 text-sm" ] [ text "No workout available." ]
        Just ( weekNum, workout ) ->
            div [ class "max-w-xl" ]
                [ div [ class "mb-5 flex items-center gap-3" ]
                    [ weekBadge weekNum
                    , h1 [ class "text-lg font-semibold tracking-wide" ] [ text (workout.workoutName ++ " (Week " ++ String.fromInt weekNum ++ ")") ]
                    ]
                , div [ class "group relative overflow-hidden rounded-2xl p-6 bg-slate-900/50 ring-1 ring-white/5 shadow-sm" ]
                    [ div [ class "absolute inset-0 opacity-0 group-hover:opacity-100 transition-opacity duration-300 pointer-events-none" ]
                        [ div [ class "absolute -inset-px bg-gradient-to-br from-sky-400/10 via-transparent to-violet-500/10 blur-md" ] [] ]
                    , span [ class "block font-semibold text-sky-300 tracking-wide mb-4" ] [ text "Exercises" ]
                    , ul [ class "flex flex-wrap gap-2" ] (List.map viewExercise workout.exercises)
                    ]
                ]

firstWorkout : Plan -> Maybe ( Int, Workout )
firstWorkout plan =
    case plan.weeks of
        [] -> Nothing
        w :: _ ->
            case w.workouts of
                [] -> Nothing
                wk :: _ -> Just ( w.weekNumber, wk )

viewWeek : Week -> List (Html msg)
viewWeek week =
    let
        header = li [ class "col-span-full text-amber-300 font-semibold tracking-wide text-lg mt-6 first:mt-0 flex items-center gap-2" ]
            [ weekBadge week.weekNumber
            , text ("Week " ++ String.fromInt week.weekNumber)
            ]
        workouts = List.map viewWorkout week.workouts
    in
    header :: workouts

viewWorkout : Workout -> Html msg
viewWorkout w =
    li [ class "group relative overflow-hidden rounded-2xl p-5 bg-slate-900/50 ring-1 ring-white/5 hover:ring-sky-400/40 shadow-sm hover:shadow-xl transition-all duration-300 backdrop-blur-sm" ]
        [ div [ class "absolute inset-0 opacity-0 group-hover:opacity-100 transition-opacity duration-300 pointer-events-none" ]
            [ div [ class "absolute -inset-px bg-gradient-to-br from-sky-400/10 via-transparent to-violet-500/10 blur-md" ] [] ]
        , span [ class "relative block font-semibold text-sky-300 group-hover:text-sky-200 tracking-wide mb-3" ] [ text w.workoutName ]
        , ul [ class "relative flex flex-wrap gap-2" ] (List.map viewExercise w.exercises)
        ]

viewExercise : Exercise -> Html msg
viewExercise ex =
    li [ class "text-[11px] leading-tight bg-slate-800/60 hover:bg-slate-700/60 border border-slate-600/40 hover:border-sky-400/40 rounded-md px-2.5 py-1 tracking-wide font-medium text-slate-300 hover:text-sky-200 transition-colors duration-150" ]
        [ text (ex.exerciseName ++ " · sets " ++ String.fromInt ex.prescribedSets) ]

loaderIcon : Html msg
loaderIcon =
    span [ class "inline-block h-4 w-4 rounded-full border-2 border-slate-500 border-t-transparent animate-spin" ] []

weekBadge : Int -> Html msg
weekBadge n =
    span [ class "inline-flex items-center justify-center h-5 w-5 text-[10px] font-bold rounded-full bg-amber-400/20 text-amber-300 ring-1 ring-amber-300/30" ] [ text (String.fromInt n) ]


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
        Http.BadUrl u ->
            "BadUrl: " ++ u

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus statusCode ->
            "BadStatus: " ++ String.fromInt statusCode

        Http.BadBody m ->
            "BadBody: " ++ m
