module WheelPicker exposing (..)

import BodyBuilder as Builder exposing (Node)
import BodyBuilder.Attributes as Attributes
import Elegant exposing (px, vh, percent, Style, deg)
import Style
import Box
import Block
import Margin
import Transform
import Position
import Flex
import Padding
import Typography
import Constants
import AnimationFrame
import Time exposing (Time)
import Task
import List.Extra
import SingleTouch
import Touch exposing (Coordinates)
import BoundedList exposing (BoundedList)
import Color
import Date exposing (Date)
import Time exposing (Time)
import List.Extra as List


---- CONSTANTS ----


dayPickerLimits : { start : Int, end : Int }
dayPickerLimits =
    { start = 1544832000000
    , end = 1547510400000
    }


defaultPickerFaces : Int
defaultPickerFaces =
    15


defaultPickerRadiusOut : Int
defaultPickerRadiusOut =
    150


msInADay : number
msInADay =
    86400000



---- INIT ----
-- Picker


initPicker : PickerId -> DataList -> Int -> Int -> Int -> Picker
initPicker pickerId dataList faces radiusOut width =
    Picker
        { id = pickerId
        , dataList = dataList
        , faces = faces
        , radiusOut = radiusOut
        , width = width
        , angle = 0
        , state = Free ( 0, 0 )
        }


defaultPicker : PickerId -> Int -> DataList -> Picker
defaultPicker pickerId width dataList =
    initPicker pickerId dataList defaultPickerFaces defaultPickerRadiusOut width


type alias DataList =
    List ( Int, String )


type alias Angle =
    Float


type alias Speed =
    -- px/ms
    Float


type alias MouseY =
    Float


initTouchesHistory : Time -> MouseY -> Angle -> TouchesHistory
initTouchesHistory time mouseY angle =
    { startMouseY = mouseY
    , startAngle = angle
    , touches = BoundedList.new 20 |> BoundedList.insert ( time, mouseY )
    }


type alias TouchesHistory =
    { startMouseY : MouseY
    , startAngle : Angle
    , touches : BoundedList ( Time, MouseY )
    }


type State
    = Free ( Time, Speed )
    | Held TouchesHistory


type PickerId
    = DayPicker
    | HourPicker
    | MinutePicker


calculateAngle : Time -> Picker -> Picker
calculateAngle currentTime (Picker picker) =
    let
        maxAngle =
            (List.length picker.dataList |> toFloat) * (pickerAngleBetweenFaces picker.faces)

        setLimit angle =
            if angle < 0 then
                0
            else if angle > maxAngle then
                maxAngle
            else
                angle

        angleFromTouches touchesHistory =
            touchesHistory.touches
                |> BoundedList.content
                |> List.take 2
                |> (\list ->
                        case list of
                            ( _, mouseY ) :: xs ->
                                touchesHistory.startAngle + 50 * (touchesHistory.startMouseY - mouseY) / (toFloat picker.radiusOut)

                            _ ->
                                picker.angle
                   )
                |> setLimit

        angleFromInertia ( speed, previousTime ) =
            0
    in
        case picker.state of
            Free data ->
                Picker picker
                    |> setAngle (angleFromInertia data)

            Held touchesHistory ->
                Picker picker
                    |> setAngle (angleFromTouches touchesHistory)


setAngle : Angle -> Picker -> Picker
setAngle angle (Picker picker) =
    Picker { picker | angle = angle }


setState : State -> Picker -> Picker
setState state (Picker picker) =
    Picker { picker | state = state }


type Picker
    = Picker
        { id : PickerId
        , dataList : DataList
        , faces : Int
        , radiusOut : Int
        , width : Int
        , angle : Angle
        , state : State

        -- , activeItem : Date
        }


initDayPicker : Picker
initDayPicker =
    let
        valueToDate value =
            value
                |> toFloat
                |> Date.fromTime

        valueToCouple value =
            ( value
            , String.join " " <|
                [ value |> valueToDate |> Date.day |> toString
                , value |> valueToDate |> Date.month |> toString
                , value |> valueToDate |> Date.year |> toString
                ]
            )
    in
        dateRange dayPickerLimits.start dayPickerLimits.end
            |> List.map valueToCouple
            |> defaultPicker DayPicker 175


initHourPicker : Picker
initHourPicker =
    let
        valueToCouple value =
            ( value, intToString 2 value )
    in
        List.range 0 23
            |> List.map valueToCouple
            |> defaultPicker HourPicker 60


initMinutePicker : Picker
initMinutePicker =
    let
        valueToCouple value =
            ( value, intToString 2 value )
    in
        List.range 0 59
            |> List.map valueToCouple
            |> defaultPicker MinutePicker 60



-- Model


setPicker : PickerId -> Picker -> Model -> Model
setPicker pickerId picker model =
    case pickerId of
        DayPicker ->
            { model | dayPicker = picker }

        HourPicker ->
            { model | hourPicker = picker }

        MinutePicker ->
            { model | minutePicker = picker }


initModel : Model
initModel =
    { date = Nothing
    , dayPicker = initDayPicker
    , hourPicker = initHourPicker
    , minutePicker = initMinutePicker
    }



---- UPDATE ----


type TouchState
    = StartTouching
    | HoldTouching
    | StopTouching


type GetTouchMsg
    = StartTouch Coordinates
    | HoldTouch Coordinates
    | StopTouch Coordinates


updateRecordingTouches : PickerId -> GetTouchMsg -> Model -> ( Model, Cmd Msg )
updateRecordingTouches pickerId getTouchMsg model =
    let
        recordTouch mouseY touchState =
            Task.perform ((pickerMsgConstructor pickerId) << RecordTouch mouseY touchState) Time.now
    in
        case getTouchMsg of
            StartTouch { clientY } ->
                ( model, recordTouch clientY StartTouching )

            HoldTouch { clientY } ->
                ( model, recordTouch clientY HoldTouching )

            StopTouch { clientY } ->
                ( model, recordTouch clientY StopTouching )


updateRecordTouchAt : PickerId -> MouseY -> TouchState -> Time -> Model -> ( Model, Cmd Msg )
updateRecordTouchAt pickerId mouseY touchState time model =
    let
        (Picker picker) =
            getPicker pickerId model

        injectInModel newPicker =
            setPicker pickerId newPicker model

        newState =
            case picker.state of
                Held touchesHistory ->
                    touchesHistory
                        |> addToTouchesHistory ( time, mouseY )
                        |> Held

                _ ->
                    Held (initTouchesHistory time mouseY picker.angle)
    in
        case touchState of
            StartTouching ->
                ( Picker picker
                    |> setState newState
                    |> injectInModel
                , Cmd.none
                )

            HoldTouching ->
                ( Picker picker
                    |> setState newState
                    |> calculateAngle time
                    |> injectInModel
                , Cmd.none
                )

            StopTouching ->
                ( Picker picker
                    |> setState newState
                    |> calculateAngle time
                    |> setState (releasePickerState picker.state)
                    |> injectInModel
                , Cmd.none
                )


type PickerMsg
    = GetTouch GetTouchMsg
    | RecordTouch MouseY TouchState Time


updatePicker : PickerId -> PickerMsg -> Model -> ( Model, Cmd Msg )
updatePicker pickerId pickerMsg model =
    case pickerMsg of
        GetTouch getTouchMsg ->
            updateRecordingTouches pickerId getTouchMsg model

        RecordTouch mouseY touchState currentTime ->
            updateRecordTouchAt pickerId mouseY touchState currentTime model


type Msg
    = DayPickerMsg PickerMsg
    | HourPickerMsg PickerMsg
    | MinutePickerMsg PickerMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayPickerMsg pickerMsg ->
            updatePicker DayPicker pickerMsg model

        HourPickerMsg pickerMsg ->
            updatePicker HourPicker pickerMsg model

        MinutePickerMsg pickerMsg ->
            updatePicker MinutePicker pickerMsg model



---- SUBSCRIPTIONS ----
---- VIEW ----


pickerView : Picker -> List (Node msg)
pickerView (Picker picker) =
    let
        faceHeight =
            pickerFaceHeight picker.radiusOut picker.faces

        elementsToDrop faceIndex =
            (((toFloat faceIndex) * (pickerAngleBetweenFaces picker.faces) + picker.angle) / (pickerAngleBetweenFaces picker.faces)) |> floor

        selectionToString ( _, date, _ ) =
            String.join " " <|
                [ toString (Date.day date)
                , toString (Date.month date)
                , toString (Date.year date)
                ]

        pickerViewFace faceIndex =
            Builder.div
                [ Attributes.style
                    [ Style.blockProperties
                        [ Block.height (px (round faceHeight))
                        , Block.width (px picker.width)
                        ]
                    , Style.box
                        [ Box.position (Position.absolute [ Position.top (px ((toFloat picker.radiusOut) - 0.5 * faceHeight |> round)) ])
                        , Box.transform
                            [ Transform.rotateX (deg ((toFloat faceIndex) * (pickerAngleBetweenFaces picker.faces) |> negate))
                            , Transform.translateZ (px (pickerRadiusIn picker.faces faceHeight |> round))
                            , Transform.backfaceVisibilityHidden
                            ]
                        ]
                    ]
                ]
                [ if (elementsToDrop faceIndex) < 0 then
                    Builder.text ""
                  else
                    picker.dataList
                        |> List.drop (elementsToDrop faceIndex)
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault ""
                        |> Builder.text
                ]
    in
        List.map pickerViewFace (List.range ((toFloat picker.faces) / 2 |> floor |> negate) ((toFloat picker.faces) / 2 |> floor))


pickerContainerView : Picker -> Builder.FlexItem Msg
pickerContainerView ((Picker picker) as pickerConstructor) =
    let
        faceHeight =
            pickerFaceHeight picker.radiusOut picker.faces

        fontSize =
            pickerFontSize faceHeight

        touchMsgWrapper =
            (pickerMsgConstructor picker.id) << GetTouch
    in
        Builder.flexItem
            [ Attributes.style
                [ Style.box
                    [ Box.transform
                        [ Transform.perspective (px 1000)
                        , Transform.perspectiveOrigin ( percent 50, percent 50 )
                        , Transform.preserve3d
                        ]
                    ]
                ]
            ]
            [ Builder.div
                [ Attributes.style
                    [ Style.blockProperties
                        [ Block.height (px (2 * picker.radiusOut))
                        , Block.width (px picker.width)
                        ]
                    , Style.box
                        [ Box.transform
                            [ Transform.rotateX (deg picker.angle)
                            , Transform.preserve3d
                            , Transform.origin ( px 0, px picker.radiusOut, px (negate picker.radiusOut) )
                            , Transform.translateZ (px (negate picker.radiusOut))
                            ]
                        , Box.typography
                            [ Typography.size (px fontSize)
                            , Typography.lineHeight (px (round faceHeight))
                            ]
                        ]
                    ]
                , Attributes.rawAttribute (SingleTouch.onStart (touchMsgWrapper << StartTouch))
                , Attributes.rawAttribute (SingleTouch.onMove (touchMsgWrapper << HoldTouch))
                , Attributes.rawAttribute (SingleTouch.onEnd (touchMsgWrapper << StopTouch))
                ]
                (pickerView pickerConstructor)
            ]


view : Model -> Node Msg
view model =
    let
        pickerLabel text =
            Builder.flexItem
                [ Attributes.style
                    [ Style.box [ Box.typography [ Typography.size (px 31) ] ] ]
                ]
                [ Builder.text text ]
    in
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
                [ pickerContainerView model.dayPicker
                , pickerLabel " at "
                , pickerContainerView model.hourPicker
                , pickerLabel ":"
                , pickerContainerView model.minutePicker
                ]
            ]



---- MAIN ----


main : Program Never Model Msg
main =
    Builder.program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



---- HELPERS ----


releasePickerState : State -> State
releasePickerState state =
    let
        calculateSpeedCouple ( lastState, firstState ) =
            ( Tuple.first lastState
            , if ((Tuple.first lastState) - (Tuple.first firstState)) > 500 then
                0
              else
                ((Tuple.second lastState) - (Tuple.second firstState)) / ((Tuple.first lastState) - (Tuple.first firstState))
            )

        touchesSample touches =
            ( touches
                |> BoundedList.head
                |> Maybe.withDefault ( 0, 0 )
            , touches
                |> BoundedList.content
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( 0, 0 )
            )
    in
        case state of
            Held touchesHistory ->
                touchesHistory.touches
                    |> touchesSample
                    |> calculateSpeedCouple
                    |> Free

            _ ->
                state


computeNewSpeed : Speed -> Time -> Time -> Speed
computeNewSpeed speed currentTime lastTime =
    speed * (0.99 ^ toFloat ((round (currentTime - lastTime)) % 17))


insignificantSpeed : Speed -> Bool
insignificantSpeed speed =
    abs speed < 0.04


addToTouchesHistory : ( Time, MouseY ) -> TouchesHistory -> TouchesHistory
addToTouchesHistory touchCouple ({ touches } as touchesHistory) =
    { touchesHistory | touches = BoundedList.insert touchCouple touches }


pickerMsgConstructor : PickerId -> PickerMsg -> Msg
pickerMsgConstructor pickerId =
    case pickerId of
        DayPicker ->
            DayPickerMsg

        HourPicker ->
            HourPickerMsg

        MinutePicker ->
            MinutePickerMsg


getPicker : PickerId -> Model -> Picker
getPicker pickerId =
    case pickerId of
        DayPicker ->
            .dayPicker

        HourPicker ->
            .hourPicker

        MinutePicker ->
            .minutePicker


pickerAngleBetweenFaces : Int -> Float
pickerAngleBetweenFaces facesNb =
    360 / (toFloat facesNb)


pickerFaceHeight : Int -> Int -> Float
pickerFaceHeight pickerRadiusOut pickerFaces =
    2 * (toFloat pickerRadiusOut) * sin (pi / (toFloat pickerFaces))


pickerRadiusIn : Int -> Float -> Float
pickerRadiusIn pickerFaces pickerFaceHeight =
    pickerFaceHeight / (2 * tan (pi / (toFloat pickerFaces)))


pickerFontSize : Float -> Int
pickerFontSize pickerFaceHeight =
    0.5 * pickerFaceHeight |> round


intToString : Int -> a -> String
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
        dateRange_ (start + msInADay) end (start :: acc)
    else
        acc


dateRange : Int -> Int -> List Int
dateRange start end =
    dateRange_ start end []
