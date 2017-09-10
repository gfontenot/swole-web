module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Json.Decode

onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
  Html.Events.on "change" (Json.Decode.map tagger Html.Events.targetValue)

type WeightUnit
    = Pounds
    | Kilos

type alias Weight =
    { amount : Int
    , unit : WeightUnit
    }

type alias WorkoutSet =
    { reps : List Int
    , weight : Weight
    }

type Msg
    = RepsUpdated String
    | WeightUpdated Int
    | WeightUnitUpdated WeightUnit

initialWorkoutSet : WorkoutSet
initialWorkoutSet =
    { reps = []
    , weight =
        { amount = 0
        , unit = Kilos
        }
    }

view : WorkoutSet -> Html Msg
view set =
    div []
        [ input [type_ "text", placeholder "reps", onInput RepsUpdated ] []
        , input [type_ "text", placeholder "weight", onInput (WeightUpdated << parseInt)] []
        , select [ onChange (WeightUnitUpdated << parseUnit) ]
            [ viewUnit set.weight Pounds
            , viewUnit set.weight Kilos
            ]
        , text <| format set
        ]

viewUnit : Weight -> WeightUnit -> Html Msg
viewUnit current unit = option
    [ value <| toString unit
    , selected <| current.unit == unit
    ]
    [ text <| toString unit ]

update : Msg -> WorkoutSet -> WorkoutSet
update msg set = case msg of
    RepsUpdated str ->
        { set | reps = parseReps str }
    WeightUpdated n ->
        let
            current = set.weight
        in
            { set | weight = { current | amount = n } }
    WeightUnitUpdated unit ->
        let
            current = set.weight
        in
           { set | weight = { current | unit = unit }}

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
    in
       formattedReps ++ " @ " ++ formattedWeight set.weight

formattedWeight : Weight -> String
formattedWeight weight =
    let
        formattedUnit = case weight.unit of
            Pounds -> "lbs"
            Kilos -> "kgs"
    in
       toString weight.amount ++ " " ++ formattedUnit

main : Program Never WorkoutSet Msg
main = Html.beginnerProgram
    { model = initialWorkoutSet
    , view = view
    , update = update
    }
