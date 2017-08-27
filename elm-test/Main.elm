module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

type WeightUnit
    = Pounds
    | Kilos

type alias WorkoutSet =
    { reps : List Int
    , weight : Int
    , unitOfMeasurement : WeightUnit
    }

type Msg
    = RepsUpdated String
    | WeightUpdated String
    | UnitUpdated WeightUnit

initialWorkoutSet : WorkoutSet
initialWorkoutSet =
    { reps = []
    , weight = 0
    , unitOfMeasurement = Kilos
    }

view : WorkoutSet -> Html Msg
view set =
    div []
        [ input [type_ "text", placeholder "reps", onInput RepsUpdated ] []
        , input [type_ "text", placeholder "weight", onInput WeightUpdated] []
        , select [ onInput (UnitUpdated << parseUnit) ]
            [ viewUnit set.unitOfMeasurement Pounds
            , viewUnit set.unitOfMeasurement Kilos
            ]
        , text <| format set
        ]

viewUnit : WeightUnit -> WeightUnit -> Html Msg
viewUnit current unit = option
    [ value <| toString unit
    , selected <| current == unit
    ]
    [ text <| toString unit ]

update : Msg -> WorkoutSet -> WorkoutSet
update msg set = case msg of
    RepsUpdated str ->
        { set | reps = parseReps str }
    WeightUpdated str ->
        { set | weight = parseInt str }
    UnitUpdated unit ->
        { set | unitOfMeasurement = unit }

parseReps : String -> List Int
parseReps str
    =  String.split "+" str
    |> List.map String.trim
    |> List.map parseInt

parseInt : String -> Int
parseInt s
    = String.toInt s
    |> Result.withDefault 0

parseUnit : String -> WeightUnit
parseUnit str = case str of
    "Pounds" -> Pounds
    "Kilos" -> Kilos
    _ -> Debug.crash "Not a valid unit of measurement"

format : WorkoutSet -> String
format set =
    let
        formattedReps = String.join " + " <| List.map toString set.reps
        formattedWeight = toString set.weight
        formattedUnits = case set.unitOfMeasurement of
            Pounds -> "lbs"
            Kilos -> "kgs"
    in
       formattedReps ++ " @ " ++ formattedWeight ++ formattedUnits

main : Program Never WorkoutSet Msg
main = Html.beginnerProgram
    { model = initialWorkoutSet
    , view = view
    , update = update
    }
