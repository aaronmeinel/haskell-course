module Main exposing (main)

import Browser
import Platform.Sub as Sub
import Html exposing (Html, div, h1, text, ul, li, span, nav, button, input)
import Html.Attributes exposing (class, type_, value, placeholder, disabled)
import Html.Events exposing (onClick, onInput)
import Json.Encode as E
import Http
import Json.Decode as D
import Dict exposing (Dict)

-- MODEL

type alias SetPerf =
    { weight : Maybe Float
    , reps : Maybe Int
    }

type alias Exercise =
    { exerciseName : String
    , muscleGroup : String
    , prescribedSets : Int
    , prescribedReps : Maybe Int
    , sets : List SetPerf
    }


setDecoder : D.Decoder SetPerf
setDecoder =
    D.map2 SetPerf
        (D.field "weight" (D.nullable D.float))
        (D.field "reps" (D.nullable D.int))

exerciseDecoder : D.Decoder Exercise
exerciseDecoder =
    D.map5 Exercise
        (D.field "exerciseName" D.string)
        (D.field "muscleGroup" D.string)
        (D.field "prescribedSets" D.int)
        (D.field "prescribedReps" (D.nullable D.int))
        (D.field "sets" (D.list setDecoder))


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

type alias SetInput =
    { weight : String
    , reps : String
    , logged : Bool
    }

-- We store per-set input state in a dictionary keyed by weekIdx:workoutIdx:exerciseIdx:setIdx
-- This prevents different workouts that have an exercise at the same index from sharing input state.
type alias Inputs = Dict String SetInput

type alias Submitting =
    { weekIdx : Int
    , workoutIdx : Int
    , exerciseIdx : Int
    , setIdx : Int
    }

type alias Model =
    { status : Status
    , route : Route
    , inputs : Inputs
    , logSubmitting : Maybe Submitting -- currently submitting set
    , logError : Maybe String
    , currentWeekIndex : Int
    , currentWorkoutIndex : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Loading, route = Current, inputs = Dict.empty, logSubmitting = Nothing, logError = Nothing, currentWeekIndex = 0, currentWorkoutIndex = 0 }
    , fetchPlan
    )


-- UPDATE

type Msg
    = GotPlan (Result Http.Error Plan)
    | SetRoute Route
    | SetCurrent Int Int
    | EditSetWeight Int Int String
    | EditSetReps Int Int String
    | LogSet Int Int
    | LoggedSet Int Int (Result Http.Error ())

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPlan (Ok plan) ->
            let
                inputsDict = buildInputsFromPlan plan
            in
            ( { model | status = Loaded plan, inputs = inputsDict }, Cmd.none )

        GotPlan (Err err) ->
            ( { model | status = Failure (debugHttp err) }, Cmd.none )

        SetRoute r ->
            ( { model | route = r }, Cmd.none )

        SetCurrent wIdx wkIdx ->
            ( { model | currentWeekIndex = wIdx, currentWorkoutIndex = wkIdx, route = Current }
            , Cmd.none
            )

        EditSetWeight exIdx setIdx txt ->
            let
                key = inputKey model.currentWeekIndex model.currentWorkoutIndex exIdx setIdx
                updated = Dict.update key (\maybeSI -> Just (updateWeight txt maybeSI)) model.inputs
            in
            ( { model | inputs = updated }, Cmd.none )

        EditSetReps exIdx setIdx txt ->
            let
                key = inputKey model.currentWeekIndex model.currentWorkoutIndex exIdx setIdx
                updated = Dict.update key (\maybeSI -> Just (updateReps txt maybeSI)) model.inputs
            in
            ( { model | inputs = updated }, Cmd.none )

        LogSet exIdx setIdx ->
            case model.status of
                Loaded plan ->
                    let
                        body = buildSetLogBody model plan exIdx setIdx model.inputs
                        submittingKey = { weekIdx = model.currentWeekIndex, workoutIdx = model.currentWorkoutIndex, exerciseIdx = exIdx, setIdx = setIdx }
                    in
                    ( { model | logSubmitting = Just submittingKey, logError = Nothing }
                    , postSetLog exIdx setIdx body
                    )
                _ -> ( model, Cmd.none )

        LoggedSet exIdx setIdx (Ok _) ->
            let
                key = inputKey model.currentWeekIndex model.currentWorkoutIndex exIdx setIdx
                updated = Dict.update key (\maybeSI -> Just (markLogged maybeSI)) model.inputs
            in
            ( { model | inputs = updated, logSubmitting = Nothing }
            , fetchPlan
            )

        LoggedSet _ _ (Err e) ->
            ( { model | logSubmitting = Nothing, logError = Just (debugHttp e) }, Cmd.none )


fetchPlan : Cmd Msg
fetchPlan =
    Http.get { url = "/api/plan", expect = Http.expectJson GotPlan planDecoder }

postSetLog : Int -> Int -> E.Value -> Cmd Msg
postSetLog exIdx setIdx body =
    Http.post
        { url = "/api/logSet"
        , body = Http.jsonBody body
        , expect = Http.expectWhatever (\result -> LoggedSet exIdx setIdx result)
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
                            ul [ class "grid gap-7 md:grid-cols-2 xl:grid-cols-3" ] (List.concat (List.indexedMap viewWeek plan.weeks))

                        Current ->
                            viewCurrent model plan
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

viewCurrent : Model -> Plan -> Html Msg
viewCurrent model plan =
    let
        maybeWorkout =
            getWorkout model.currentWeekIndex model.currentWorkoutIndex plan
    in
    case maybeWorkout of
        Nothing ->
            div [ class "text-slate-400 text-sm" ] [ text "Selected workout not found." ]
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
                    , ul [ class "space-y-3" ] (List.indexedMap (viewExerciseLog model) workout.exercises)
                    ]
                ]

getWorkout : Int -> Int -> Plan -> Maybe ( Int, Workout )
getWorkout wIdx wkIdx plan =
    case List.drop wIdx plan.weeks of
        w :: _ ->
            case List.drop wkIdx w.workouts of
                wk :: _ -> Just ( w.weekNumber, wk )
                [] -> Nothing
        [] -> Nothing

-- Find the first (weekIdx, workoutIdx) where at least one exercise not fully logged.
findFirstIncomplete : Plan -> ( Int, Int )
findFirstIncomplete plan =
    let
        isLogged : Exercise -> Bool
        isLogged ex =
            case ex.sets of
                [] -> False
                ss -> List.all (\s -> s.reps /= Nothing && s.weight /= Nothing) ss

        scanWeeks : Int -> List Week -> ( Int, Int )
        scanWeeks wIdx weeks =
            case weeks of
                [] -> ( 0, 0 ) -- default fallback
                w :: restW ->
                    case scanWorkouts wIdx 0 w.workouts of
                        Just wkIdx -> ( wIdx, wkIdx )
                        Nothing -> scanWeeks (wIdx + 1) restW
        scanWorkouts : Int -> Int -> List Workout -> Maybe Int
        scanWorkouts wIdx wkIdx workouts =
            case workouts of
                [] -> Nothing
                wo :: restWo ->
                    if List.all isLogged wo.exercises then
                        scanWorkouts wIdx (wkIdx + 1) restWo
                    else
                        Just wkIdx
    in
    scanWeeks 0 plan.weeks

viewWeek : Int -> Week -> List (Html Msg)
viewWeek wIdx week =
    let
        header =
            li [ class "col-span-full text-amber-300 font-semibold tracking-wide text-lg mt-6 first:mt-0 flex items-center gap-2" ]
                [ weekBadge week.weekNumber
                , text ("Week " ++ String.fromInt week.weekNumber)
                ]

        workouts =
            List.indexedMap (viewWorkout wIdx) week.workouts
    in
    header :: workouts

viewWorkout : Int -> Int -> Workout -> Html Msg
viewWorkout wIdx wkIdx w =
    li [ class "cursor-pointer group relative overflow-hidden rounded-2xl p-5 bg-slate-900/50 ring-1 ring-white/5 hover:ring-sky-400/40 shadow-sm hover:shadow-xl transition-all duration-300 backdrop-blur-sm"
       , onClick (SetCurrent wIdx wkIdx)
       ]
        [ div [ class "absolute inset-0 opacity-0 group-hover:opacity-100 transition-opacity duration-300 pointer-events-none" ]
            [ div [ class "absolute -inset-px bg-gradient-to-br from-sky-400/10 via-transparent to-violet-500/10 blur-md" ] [] ]
        , span [ class "relative block font-semibold text-sky-300 group-hover:text-sky-200 tracking-wide mb-3" ] [ text w.workoutName ]
        , ul [ class "relative flex flex-wrap gap-2" ] (List.map viewExercise w.exercises)
        ]

viewExercise : Exercise -> Html msg
viewExercise ex =
    li [ class "text-[11px] leading-tight bg-slate-800/60 hover:bg-slate-700/60 border border-slate-600/40 hover:border-sky-400/40 rounded-md px-2.5 py-1 tracking-wide font-medium text-slate-300 hover:text-sky-200 transition-colors duration-150" ]
        [ text (ex.exerciseName ++ " · sets " ++ String.fromInt ex.prescribedSets) ]

viewExerciseLog : Model -> Int -> Exercise -> Html Msg
viewExerciseLog model exIdx ex =
    let
        rirLabel = "RIR 3 (initial)"
        completed =
            let
                setStatuses =
                    ex.sets
                        |> List.indexedMap (\setIdx _ ->
                            let si = getSetInput model.currentWeekIndex model.currentWorkoutIndex exIdx setIdx model.inputs
                            in si.logged
                           )
            in
            List.all identity setStatuses
    in
    li [ class "flex flex-col gap-3 rounded-xl border border-slate-700/40 bg-slate-800/40 p-4" ]
        [ span [ class "text-sm font-medium text-sky-200" ] [ text ex.exerciseName ]
        , span [ class "text-[11px] uppercase tracking-wide text-slate-500" ] [ text rirLabel ]
        , if completed then span [ class "text-[11px] text-emerald-400 font-medium" ] [ text "All sets logged" ] else text ""
    , div [ class "flex flex-col gap-2" ] (List.indexedMap (viewSetRow model exIdx) ex.sets)
        ]

viewSetRow : Model -> Int -> Int -> SetPerf -> Html Msg
viewSetRow model exIdx setIdx _ =
    let
        inputRec = getSetInput model.currentWeekIndex model.currentWorkoutIndex exIdx setIdx model.inputs
        wVal = inputRec.weight
        rVal = inputRec.reps
        logged = inputRec.logged
        ready = (String.length wVal > 0) && (String.length rVal > 0) && not logged
    in
    div [ class "flex items-end gap-3" ]
        [ span [ class "text-xs w-10 text-slate-400" ] [ text ("Set " ++ String.fromInt (setIdx + 1)) ]
        , numberField wVal logged (EditSetWeight exIdx setIdx) "Weight"
        , numberField rVal logged (EditSetReps exIdx setIdx) "Reps"
        , setLogButton exIdx setIdx ready logged
        ]

numberField : String -> Bool -> (String -> Msg) -> String -> Html Msg
numberField current disabledAll toMsg label =
    div [ class "flex flex-col w-24" ]
        [ span [ class "text-[10px] tracking-wide text-slate-400 mb-1" ] [ text label ]
        , input
            [ class "rounded-md bg-slate-900/60 border border-slate-600/40 focus:border-sky-400/60 focus:outline-none px-2 py-1 text-sm"
            , type_ "number"
            , placeholder label
            , value current
            , disabled disabledAll
            , onInput toMsg
            ]
            []
        ]

setLogButton : Int -> Int -> Bool -> Bool -> Html Msg
setLogButton exIdx setIdx ready logged =
    button
        [ class ("mt-4 h-8 px-3 inline-flex items-center rounded-md text-xs font-medium disabled:opacity-50 disabled:cursor-not-allowed " ++
            (if logged then "bg-emerald-600" else "bg-sky-600 hover:bg-sky-500"))
        , disabled (not ready)
        , onClick (LogSet exIdx setIdx)
        ]
        [ text (if logged then "Done" else "Log") ]

-- INPUT STATE HELPERS

inputKey : Int -> Int -> Int -> Int -> String
inputKey wIdx wkIdx exIdx setIdx =
    String.fromInt wIdx ++ ":" ++ String.fromInt wkIdx ++ ":" ++ String.fromInt exIdx ++ ":" ++ String.fromInt setIdx

defaultSetInput : SetInput
defaultSetInput = { weight = "", reps = "", logged = False }

getSetInput : Int -> Int -> Int -> Int -> Inputs -> SetInput
getSetInput wIdx wkIdx exIdx setIdx inputs =
    Dict.get (inputKey wIdx wkIdx exIdx setIdx) inputs |> Maybe.withDefault defaultSetInput

updateWeight : String -> Maybe SetInput -> SetInput
updateWeight txt maybeSI =
    case maybeSI of
        Just si -> { si | weight = txt }
        Nothing -> { defaultSetInput | weight = txt }

updateReps : String -> Maybe SetInput -> SetInput
updateReps txt maybeSI =
    case maybeSI of
        Just si -> { si | reps = txt }
        Nothing -> { defaultSetInput | reps = txt }

markLogged : Maybe SetInput -> SetInput
markLogged maybeSI =
    case maybeSI of
        Just si -> { si | logged = True }
        Nothing -> { defaultSetInput | logged = True }

buildInputsFromPlan : Plan -> Inputs
buildInputsFromPlan plan =
    let
        weekList =
            plan.weeks
                |> List.indexedMap (\wIdx w ->
                    w.workouts
                        |> List.indexedMap (\wkIdx wkout ->
                            wkout.exercises
                                |> List.indexedMap (\exIdx ex ->
                                    ex.sets
                                        |> List.indexedMap (\setIdx s ->
                                            ( inputKey wIdx wkIdx exIdx setIdx
                                            , { weight = maybeToStrFloat s.weight
                                              , reps = maybeToStr s.reps
                                              , logged = (s.weight /= Nothing && s.reps /= Nothing)
                                              }
                                            )
                                        )
                                )
                        )
                )
                |> List.concatMap (List.concatMap List.concat)
    in
    Dict.fromList weekList

buildSetLogBody : Model -> Plan -> Int -> Int -> Inputs -> E.Value
buildSetLogBody model plan exIdx setIdx inputs =
    let ( weekNum, workoutIndex ) =
            case getWorkout model.currentWeekIndex model.currentWorkoutIndex plan of
                Just ( wn, _ ) -> ( wn, model.currentWorkoutIndex )
                Nothing -> ( 1, 0 )
        setInput = getSetInput model.currentWeekIndex model.currentWorkoutIndex exIdx setIdx inputs
        weightVal = String.toFloat setInput.weight |> Maybe.withDefault 0
        repsVal = String.toInt setInput.reps |> Maybe.withDefault 0
    in
    E.object
    [ ( "week", E.int weekNum )
    , ( "workoutIndex", E.int workoutIndex )
    , ( "exerciseIndex", E.int exIdx )
    , ( "setIndex", E.int setIdx )
    , ( "loggedWeight", E.float weightVal )
    , ( "loggedReps", E.int repsVal )
    ]


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

maybeToStr : Maybe Int -> String
maybeToStr m =
    case m of
        Just n -> String.fromInt n
        Nothing -> ""

maybeToStrFloat : Maybe Float -> String
maybeToStrFloat m =
    case m of
        Just n -> String.fromFloat n
        Nothing -> ""
