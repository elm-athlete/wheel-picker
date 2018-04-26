module DatePicker exposing (main)

import AnimationFrame
import Block
import BodyBuilder as Builder exposing (Node)
import BodyBuilder.Attributes as Attributes
import Box
import Date
import Elegant exposing (px)
import Flex
import Margin
import SingleTouch
import Style
import Task
import Typography
import WheelPicker as Picker


---- CONSTANTS ----


dayPickerLimits : { start : Int, end : Int }
dayPickerLimits =
    { start = 1544832000000
    , end = 1547510400000
    }


toMs =
    { day = 86400000
    , hour = 3600000
    , minute = 60000
    }



---- INIT ----


initDayPicker : Picker.WheelPicker
initDayPicker =
    let
        valueToDate value =
            value
                |> toFloat
                |> Date.fromTime

        valueToString value =
            [ value |> valueToDate |> Date.day |> toString
            , value |> valueToDate |> Date.month |> toString
            , value |> valueToDate |> Date.year |> toString
            ]
                |> String.join " "
    in
        dateRange dayPickerLimits.start dayPickerLimits.end
            |> List.map valueToString
            |> Picker.defaultWheelPicker 175


initHourPicker : Picker.WheelPicker
initHourPicker =
    List.range 0 59
        |> List.map (\value -> intToString 2 value)
        |> Picker.defaultWheelPicker 60


initMinutePicker : Picker.WheelPicker
initMinutePicker =
    List.range 0 59
        |> List.map (\value -> intToString 2 value)
        |> Picker.defaultWheelPicker 60


setDate : Date.Date -> Model -> Model
setDate date model =
    { model | date = date }


initialModel : Model
initialModel =
    { date = Date.fromTime 0
    , dayPicker = initDayPicker
    , hourPicker = initHourPicker
    , minutePicker = initMinutePicker
    }


type alias Model =
    { date : Date.Date
    , dayPicker : Picker.WheelPicker
    , hourPicker : Picker.WheelPicker
    , minutePicker : Picker.WheelPicker
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
        |> setDate (dateFromPickers initialModel)
    , Cmd.none
    )



---- UPDATE ----


type PickerId
    = DayPicker
    | HourPicker
    | MinutePicker


updatePicker : PickerId -> Picker.Msg -> Model -> ( Model, Cmd Msg )
updatePicker pickerId pickerMsg model =
    let
        updateSpecificPicker picker =
            Picker.update pickerMsg picker
    in
        case pickerId of
            DayPicker ->
                ( { model | dayPicker = Tuple.first (updateSpecificPicker model.dayPicker) }
                    |> setDate (Date.fromTime (1 * 86400000))
                  -- (dateFromPickers initialModel)
                , Cmd.map (PickerMsg DayPicker) (Tuple.second (updateSpecificPicker model.dayPicker))
                )

            HourPicker ->
                ( { model | hourPicker = Tuple.first (updateSpecificPicker model.hourPicker) }
                    |> setDate (dateFromPickers initialModel)
                , Cmd.map (PickerMsg HourPicker) (Tuple.second (updateSpecificPicker model.hourPicker))
                )

            MinutePicker ->
                ( { model | minutePicker = Tuple.first (updateSpecificPicker model.minutePicker) }
                    |> setDate (dateFromPickers initialModel)
                , Cmd.map (PickerMsg MinutePicker) (Tuple.second (updateSpecificPicker model.minutePicker))
                )


type Msg
    = PickerMsg PickerId Picker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickerMsg pickerId pickerMsg ->
            updatePicker pickerId pickerMsg model



---- SUBSCRIPTIONS ----


pickerSubscriptions : PickerId -> Picker.WheelPicker -> Sub Msg
pickerSubscriptions pickerId picker =
    case Picker.getSpeedState picker of
        Nothing ->
            Sub.none

        Just _ ->
            AnimationFrame.times (PickerMsg pickerId << Picker.NewFrame)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pickerSubscriptions DayPicker model.dayPicker
        , pickerSubscriptions HourPicker model.hourPicker
        , pickerSubscriptions MinutePicker model.minutePicker
        ]



---- VIEW ----


pickerView : PickerId -> Picker.WheelPicker -> Builder.FlexItem Msg
pickerView pickerId picker =
    let
        touchMsgWrapper =
            PickerMsg pickerId << Picker.GetTouch
    in
        Builder.flexItem
            [ Attributes.rawAttribute (SingleTouch.onStart (touchMsgWrapper << Picker.StartTouch))
            , Attributes.rawAttribute (SingleTouch.onMove (touchMsgWrapper << Picker.HoldTouch))
            , Attributes.rawAttribute (SingleTouch.onEnd (touchMsgWrapper << Picker.StopTouch))
            ]
            [ Picker.view picker ]


pickerLabelView : String -> Builder.FlexItem Msg
pickerLabelView text =
    Builder.flexItem []
        [ Builder.div
            [ Attributes.style
                [ Style.box [ Box.typography [ Typography.size (px 31) ] ] ]
            ]
            [ Builder.text text ]
        ]


view : Model -> Node Msg
view model =
    Builder.div
        [ Attributes.style
            [ Style.blockProperties [ Block.alignCenter ]
            , Style.box [ Box.margin [ Margin.top <| Margin.width (px 200) ] ]
            ]
        ]
        [ Builder.flex
            [ Attributes.style
                [ Style.flexContainerProperties [ Flex.direction Flex.row, Flex.align Flex.alignCenter ] ]
            ]
            [ pickerView DayPicker model.dayPicker
            , pickerLabelView " at "
            , pickerView HourPicker model.hourPicker
            , pickerLabelView ":"
            , pickerView MinutePicker model.minutePicker
            ]
        , Builder.div [] [ Builder.text (toString model.date) ]
        ]



---- MAIN ----


main : Program Never Model Msg
main =
    Builder.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



---- HELPERS ----


intToString : Int -> Int -> String
intToString digitsNb value =
    let
        baseString =
            toString value

        additionalZeros =
            baseString
                |> String.length
                |> (-) digitsNb
                |> flip String.repeat "0"
    in
        String.append additionalZeros baseString


dateRange_ : Int -> Int -> List Int -> List Int
dateRange_ start end acc =
    if start < end then
        dateRange_ (start + toMs.day) end (start :: acc)
    else
        acc


dateRange : Int -> Int -> List Int
dateRange start end =
    dateRange_ start end []


dateFromPickers : Model -> Date.Date
dateFromPickers model =
    let
        day =
            Picker.getSelect model.dayPicker

        hour =
            Picker.getSelect model.hourPicker

        minute =
            Picker.getSelect model.minutePicker
    in
        (toMs.day * day + toMs.hour * hour + toMs.minute * minute)
            |> toFloat
            |> Date.fromTime
