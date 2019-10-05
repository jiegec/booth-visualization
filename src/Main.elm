module Main exposing (..)

import Bitwise
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { a : Int
    , b : Int
    , bits : Int
    }


type Action
    = Init
    | Add
    | Sub
    | Shift


type alias Step =
    { step : Int
    , shiftSteps : Int
    , action : Action
    , product : Int
    , additional : Int
    , bits : Int
    }


init : Model
init =
    { a = 3
    , b = -7
    , bits = 5
    }


type Msg
    = InputA String
    | InputB String
    | InputBits String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputA a ->
            { model | a = clampInput (Maybe.withDefault 0 (String.toInt a)) model.bits }

        InputB b ->
            { model | b = clampInput (Maybe.withDefault 0 (String.toInt b)) model.bits }

        InputBits input ->
            let
                bits =
                    clamp 1 15 (Maybe.withDefault 0 (String.toInt input))
            in
            { a = clampInput model.a bits, b = clampInput model.b bits, bits = bits }


clampInput : Int -> Int -> Int
clampInput input bits =
    clamp (0 - Bitwise.shiftLeftBy bits 1 // 2) (Bitwise.shiftLeftBy bits 1 // 2 - 1) input


view : Model -> Html Msg
view model =
    let
        initStep =
            { step = 0
            , shiftSteps = 0
            , action = Init
            , product = Bitwise.and model.b (Bitwise.shiftLeftBy model.bits 1 - 1)
            , additional = 0
            , bits = model.bits
            }
    in
    div []
        [ h1 [] [ text "Visualization of Booth Multiplication" ]
        , div []
            [ text "A = "
            , input [ onInput InputA, value (String.fromInt model.a) ] []
            ]
        , showInt model.a model.bits
        , div []
            [ text "B = "
            , input [ onInput InputB, value (String.fromInt model.b) ] []
            ]
        , showInt model.b model.bits
        , div []
            [ text "Bits = "
            , input [ onInput InputBits, value (String.fromInt model.bits) ] []
            ]
        , showSteps initStep model.a model.bits
        , p [] []
        , details [] [
            summary [] [text "Algorithm"]
            , ul [] [ text "Partial Product is initialized to B (zero extension)." ]
            , ul [] [ text "For each step after Init or Shift, check the LSB of Partial Product and Additional Bit to determine the next step." ]
            , ul [] [ text "Add or Substract A shifted by Bits or Shift again." ]
            , ul [] [ text "Repeat until Shift Bits-times." ]
        ]
        , details [] [
            summary [] [text "Caveats"]
            , ul [] [ text "Partial Product has an additional sign bit to handle A = -2^{n-1} case." ]
            , ul [] [ text "Step column is counted by the number of Shift steps." ]
            ]
        ]


showInt : Int -> Int -> Html Msg
showInt number bits =
    div [] (List.map (\i -> text (String.fromInt (getBit number i))) (List.reverse (List.range 0 (bits - 1))))


showSteps : Step -> Int -> Int -> Html Msg
showSteps initStep a bits =
    let
        steps =
            allSteps initStep a bits
    in
    div []
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Step" ]
                    , th [] [ text "Sub Step" ]
                    , th [] [ text "Action" ]
                    , th [] [ text "Partial Product" ]
                    , th [] [ text "Additional Bit" ]
                    ]
                ]
            , tbody [] (List.map (\s -> showStep s) steps)
            ]
        , text "Answer is "
        , text (String.fromInt (Maybe.withDefault 0 (Maybe.map (\step -> step.product) (List.head (List.drop (List.length steps - 1) steps)))))
        ]


allSteps : Step -> Int -> Int -> List Step
allSteps initStep a bits =
    if initStep.shiftSteps == bits then
        [ initStep ]

    else
        initStep :: allSteps (nextStep initStep a) a bits


showStep : Step -> Html Msg
showStep step =
    tr []
        [ td [] [ text (String.fromInt step.shiftSteps) ]
        , td [] [ text (String.fromInt step.step) ]
        , td [] [ text (toString step.action) ]
        , td [] [ showInt step.product (step.bits * 2 + 1) ]
        , td [] [ text (String.fromInt step.additional) ]
        ]


toString : Action -> String
toString action =
    case action of
        Init ->
            "Init"

        Add ->
            "Add"

        Sub ->
            "Sub"

        Shift ->
            "Shift"


getBit : Int -> Int -> Int
getBit number bit =
    if Bitwise.and number (Bitwise.shiftLeftBy bit 1) /= 0 then
        1

    else
        0


nextStep : Step -> Int -> Step
nextStep prev a =
    case prev.action of
        Add ->
            shift prev

        Sub ->
            shift prev

        Init ->
            check prev a

        Shift ->
            check prev a


shift : Step -> Step
shift prev =
    { step = prev.step + 1
    , shiftSteps = prev.shiftSteps + 1
    , action = Shift
    , product = Bitwise.shiftRightBy 1 prev.product
    , additional = Bitwise.and prev.product 1
    , bits = prev.bits
    }


check : Step -> Int -> Step
check prev a =
    let
        lowbits =
            Bitwise.and prev.product 1 * 2 + prev.additional
    in
    if lowbits == 2 then
        { step = prev.step + 1
        , shiftSteps = prev.shiftSteps
        , action = Sub
        , product = prev.product - Bitwise.shiftLeftBy prev.bits a
        , additional = prev.additional
        , bits = prev.bits
        }

    else if lowbits == 1 then
        { step = prev.step + 1
        , shiftSteps = prev.shiftSteps
        , action = Add
        , product = prev.product + Bitwise.shiftLeftBy prev.bits a
        , additional = prev.additional
        , bits = prev.bits
        }

    else
        { step = prev.step + 1
        , shiftSteps = prev.shiftSteps + 1
        , action = Shift
        , product = Bitwise.shiftRightBy 1 prev.product
        , additional = Bitwise.and prev.product 1
        , bits = prev.bits
        }
