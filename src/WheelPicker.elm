module WheelPicker
    exposing
        (..
         --  WheelPicker
         -- , defaultWheelPicker
         -- , update
         -- , Msg(..)
        )

import BodyBuilder as Builder exposing (Node)
import BodyBuilder.Attributes as Attributes
import Elegant exposing (px, vh, percent, Style, deg)
import Style
import Box
import Block


-- import Margin

import Transform
import Position


-- import Flex
-- import Padding

import Typography


-- import Constants

import Time
import Task


-- import List.Extra as List

import Touch exposing (Coordinates)
import BoundedList exposing (BoundedList)


-- import Color

import Date exposing (Date)
import Time exposing (Time)


---- CONSTANTS ----


defaultPickerFaces : Int
defaultPickerFaces =
    15


defaultPickerRadiusOut : Int
defaultPickerRadiusOut =
    150


touchesHistoryLength : Int
touchesHistoryLength =
    20


approachSpeed : Float
approachSpeed =
    0.05


friction : Float
friction =
    0.997



---- MODEL ----
{- Sub-Models -}


type alias DataList =
    List String


type alias Angle =
    Float


type State
    = Free
    | Held
    | Approach


type alias Speed =
    -- deg/ms
    Float


type alias SpeedState =
    -- ( ( t-1, t ), deg/ms )
    ( ( Maybe Time, Time ), Speed )


type alias MouseY =
    Float


initTouchesHistory : Time -> MouseY -> Angle -> Maybe TouchesHistory
initTouchesHistory time mouseY angle =
    Just
        { startMouseY = mouseY
        , startAngle = angle
        , touches = BoundedList.new touchesHistoryLength |> BoundedList.insert ( time, mouseY )
        }


addToTouchesHistory : ( Time, MouseY ) -> Maybe TouchesHistory -> Maybe TouchesHistory
addToTouchesHistory touch maybeTouchesHistory =
    case maybeTouchesHistory of
        Nothing ->
            Nothing

        Just touchesHistory ->
            Just { touchesHistory | touches = BoundedList.insert touch touchesHistory.touches }


type alias TouchesHistory =
    { startMouseY : MouseY
    , startAngle : Angle
    , touches : BoundedList ( Time, MouseY )
    }



{- Model -}


setAngle : Angle -> WheelPicker -> WheelPicker
setAngle angle ((WheelPicker picker) as wheelPicker) =
    WheelPicker { picker | angle = angle }


setAngleFromTouches : WheelPicker -> WheelPicker
setAngleFromTouches wheelPicker =
    wheelPicker
        |> setAngle (angleFromTouches wheelPicker)
        |> applyLimitAngles


setAngleFromSpeed : WheelPicker -> WheelPicker
setAngleFromSpeed wheelPicker =
    wheelPicker
        |> detectApproach
        |> setAngle (angleFromSpeed wheelPicker)
        |> applyLimitAngles


setSpeedStateFromNewFrame : Time -> WheelPicker -> WheelPicker
setSpeedStateFromNewFrame currentTime wheelPicker =
    wheelPicker
        |> setSpeedState (speedStateFromNewFrame currentTime wheelPicker)


setSpeedStateFromTouches : WheelPicker -> WheelPicker
setSpeedStateFromTouches wheelPicker =
    wheelPicker
        |> setSpeedState (speedStateFromTouches wheelPicker)


getSpeedState : WheelPicker -> Maybe SpeedState
getSpeedState (WheelPicker picker) =
    picker.speedState


setSpeedState : Maybe SpeedState -> WheelPicker -> WheelPicker
setSpeedState speedState (WheelPicker picker) =
    WheelPicker { picker | speedState = speedState }


setState : State -> WheelPicker -> WheelPicker
setState state (WheelPicker picker) =
    WheelPicker { picker | state = state }


setTouchesHistory : Maybe TouchesHistory -> WheelPicker -> WheelPicker
setTouchesHistory touchesHistory (WheelPicker picker) =
    WheelPicker { picker | touchesHistory = touchesHistory }


getSelect : WheelPicker -> Int
getSelect (WheelPicker picker) =
    picker.select


setSelect : Int -> WheelPicker -> WheelPicker
setSelect select (WheelPicker picker) =
    WheelPicker { picker | select = select }


initWheelPicker : DataList -> Int -> Int -> Int -> WheelPicker
initWheelPicker dataList faces radiusOut width =
    WheelPicker
        { angle = 0
        , dataList = dataList
        , faces = faces
        , radiusOut = radiusOut
        , select = 0
        , speedState = Nothing
        , state = Free
        , touchesHistory = Nothing
        , width = width
        }


defaultWheelPicker : Int -> DataList -> WheelPicker
defaultWheelPicker width dataList =
    initWheelPicker dataList defaultPickerFaces defaultPickerRadiusOut width


type WheelPicker
    = WheelPicker
        { angle : Angle
        , dataList : DataList
        , faces : Int
        , radiusOut : Int
        , select : Int
        , speedState : Maybe SpeedState
        , state : State
        , touchesHistory : Maybe TouchesHistory
        , width : Int
        }



---- UPDATE ----


type GetTouchMsg
    = StartTouch Coordinates
    | HoldTouch Coordinates
    | StopTouch Coordinates


updateGetTouch : GetTouchMsg -> WheelPicker -> ( WheelPicker, Cmd Msg )
updateGetTouch getTouchMsg wheelPicker =
    let
        recordTouch mouseY touchState =
            Task.perform (RecordTouch mouseY touchState) Time.now
    in
        case getTouchMsg of
            StartTouch { clientY } ->
                ( wheelPicker, recordTouch clientY StartTouching )

            HoldTouch { clientY } ->
                ( wheelPicker, recordTouch clientY HoldTouching )

            StopTouch { clientY } ->
                ( wheelPicker, recordTouch clientY StopTouching )


type TouchState
    = StartTouching
    | HoldTouching
    | StopTouching


updateRecordTouch : MouseY -> TouchState -> Time -> WheelPicker -> ( WheelPicker, Cmd Msg )
updateRecordTouch mouseY touchState currentTime (WheelPicker picker) =
    case touchState of
        StartTouching ->
            (WheelPicker picker
                |> setSpeedState Nothing
                |> setTouchesHistory (initTouchesHistory currentTime mouseY picker.angle)
                |> setState Held
            )
                ! []

        HoldTouching ->
            ( WheelPicker picker
                |> setTouchesHistory (addToTouchesHistory ( currentTime, mouseY ) picker.touchesHistory)
                |> setAngleFromTouches
            , Cmd.none
            )

        StopTouching ->
            ( WheelPicker picker
                |> setTouchesHistory (addToTouchesHistory ( currentTime, mouseY ) picker.touchesHistory)
                |> setState Free
                |> setSpeedStateFromTouches
                |> setTouchesHistory Nothing
                |> setAngleFromSpeed
            , Cmd.none
            )


updateNewFrame : Time -> WheelPicker -> ( WheelPicker, Cmd Msg )
updateNewFrame currentTime (WheelPicker picker) =
    (WheelPicker picker
        |> setSpeedStateFromNewFrame currentTime
        |> setAngleFromSpeed
    )
        ! []


type Msg
    = GetTouch GetTouchMsg
    | RecordTouch MouseY TouchState Time
    | NewFrame Time


update : Msg -> WheelPicker -> ( WheelPicker, Cmd Msg )
update msg wheelPicker =
    let
        updateSelect ( newPicker, newCmdMsg ) =
            ( newPicker
                |> setSelect (resolveSelect wheelPicker)
            , newCmdMsg
            )
    in
        updateSelect <|
            (case msg of
                GetTouch getTouchMsg ->
                    updateGetTouch getTouchMsg wheelPicker

                RecordTouch mouseY touchState currentTime ->
                    updateRecordTouch mouseY touchState currentTime wheelPicker

                NewFrame currentTime ->
                    updateNewFrame currentTime wheelPicker
            )



---- VIEW ----


wheelPickerView : WheelPicker -> List (Node msg)
wheelPickerView (WheelPicker picker) =
    let
        pickerFaceHeight =
            faceHeight picker.radiusOut picker.faces

        elementsToDrop faceIndex =
            (((toFloat faceIndex) * (angleBetweenFaces picker.faces) + picker.angle) / (angleBetweenFaces picker.faces)) |> floor

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
                        [ Block.height (px (round pickerFaceHeight))
                        , Block.width (px picker.width)
                        ]
                    , Style.box
                        [ Box.position (Position.absolute [ Position.top (px ((toFloat picker.radiusOut) - 0.5 * pickerFaceHeight |> round)) ])
                        , Box.transform
                            [ Transform.rotateX (deg ((toFloat faceIndex) * (angleBetweenFaces picker.faces) |> negate))
                            , Transform.translateZ (px (radiusIn picker.faces pickerFaceHeight |> round))
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
                        |> Maybe.withDefault ""
                        |> Builder.text
                ]
    in
        List.map pickerViewFace (List.range ((toFloat picker.faces) / 2 |> floor |> negate) ((toFloat picker.faces) / 2 |> floor))


view : WheelPicker -> Node msg
view ((WheelPicker picker) as wheelPicker) =
    let
        pickerFaceHeight =
            faceHeight picker.radiusOut picker.faces

        pickerFontSize =
            fontSize pickerFaceHeight
    in
        Builder.div
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
                            [ Typography.size (px pickerFontSize)
                            , Typography.lineHeight (px (round pickerFaceHeight))
                            ]
                        ]
                    ]
                ]
                (wheelPickerView wheelPicker)
            ]



---- HELPERS ----


angleBetweenFaces : Int -> Float
angleBetweenFaces facesNb =
    360 / (toFloat facesNb)


faceHeight : Int -> Int -> Float
faceHeight pickerRadiusOut pickerFaces =
    2 * (toFloat pickerRadiusOut) * sin (pi / (toFloat pickerFaces))


radiusIn : Int -> Float -> Float
radiusIn pickerFaces pickerFaceHeight =
    pickerFaceHeight / (2 * tan (pi / (toFloat pickerFaces)))


fontSize : Float -> Int
fontSize pickerFaceHeight =
    0.5 * pickerFaceHeight |> round


degPerPx : Int -> Float
degPerPx radius =
    360 / (2 * pi * toFloat radius)


applyLimitAngles : WheelPicker -> WheelPicker
applyLimitAngles (WheelPicker picker) =
    let
        maxAngle =
            ((List.length picker.dataList |> toFloat) - 1) * (angleBetweenFaces picker.faces)
    in
        if picker.angle < 0 then
            WheelPicker picker
                |> setAngle 0
        else if picker.angle > maxAngle then
            WheelPicker picker
                |> setAngle maxAngle
        else
            WheelPicker picker


angleFromTouches : WheelPicker -> Angle
angleFromTouches (WheelPicker picker) =
    let
        newAngle touchesHistory =
            touchesHistory.touches
                |> BoundedList.content
                |> List.take 2
                |> (\list ->
                        case list of
                            ( _, mouseY ) :: xs ->
                                touchesHistory.startAngle + (degPerPx picker.radiusOut) * (touchesHistory.startMouseY - mouseY)

                            _ ->
                                picker.angle
                   )
    in
        case picker.touchesHistory of
            Nothing ->
                picker.angle

            Just touchesHistory ->
                newAngle touchesHistory


speedStateFromNewFrame : Time -> WheelPicker -> Maybe SpeedState
speedStateFromNewFrame currentTime (WheelPicker picker) =
    let
        newSpeed ( ( _, previousTime ), previousSpeed ) =
            previousSpeed * friction ^ (currentTime - previousTime)
    in
        case picker.speedState of
            Nothing ->
                Nothing

            Just (( ( _, lastTime ), _ ) as speedState) ->
                Just
                    ( ( Just lastTime, currentTime )
                    , newSpeed speedState
                    )


angleFromSpeed : WheelPicker -> Angle
angleFromSpeed (WheelPicker picker) =
    case picker.speedState of
        Nothing ->
            picker.angle

        Just ( ( maybePreviousTime, lastTime ), speed ) ->
            case maybePreviousTime of
                Nothing ->
                    picker.angle

                Just previousTime ->
                    picker.angle + speed * (lastTime - previousTime)


speedStateFromTouches : WheelPicker -> Maybe SpeedState
speedStateFromTouches (WheelPicker picker) =
    let
        calculateSpeedState ( ( lastTime, lastMouseY ), ( firstTime, firstMouseY ) ) =
            ( ( Nothing, lastTime )
            , if (lastTime - firstTime) > 500 then
                0
              else
                (degPerPx picker.radiusOut) * (firstMouseY - lastMouseY) / (lastTime - firstTime)
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
        case picker.touchesHistory of
            Nothing ->
                Nothing

            Just touchesHistory ->
                touchesHistory.touches
                    |> touchesSample
                    |> calculateSpeedState
                    |> Just


detectApproach : WheelPicker -> WheelPicker
detectApproach (WheelPicker picker) =
    case picker.state of
        Free ->
            case picker.speedState of
                Just ( times, speed ) ->
                    if abs speed <= approachSpeed then
                        WheelPicker picker
                            |> setState Approach
                            |> setSpeedState
                                (if speed < 0 then
                                    Just ( times, negate approachSpeed )
                                 else
                                    Just ( times, approachSpeed )
                                )
                    else
                        WheelPicker picker

                Nothing ->
                    WheelPicker picker

        _ ->
            WheelPicker picker


resolveSelect : WheelPicker -> Int
resolveSelect (WheelPicker picker) =
    picker.angle / (angleBetweenFaces picker.faces) |> round



-- calculateAngle : WheelPicker -> Angle
-- calculateAngle (WheelPicker picker) =
--     let
--
--
--         angleFromTouchesHistory touchesHistory =
--
--                    )
--         angleFrom
--     in
--         case picker.state of
--             Held ->
--                 case picker.touchesHistory of
--                     Nothing ->
--                         picker.angle
--
--                     Just touchesHistory ->
--                         touchesHistory
--                             |> angleFromTouchesHistory
--                             |> setLimit
--
--             Free ->
--                 0
-- ---- INIT ----
--
--
-- initTouchesHistory : Time -> MouseY -> Angle -> TouchesHistory
-- initTouchesHistory time mouseY angle =
--     { startMouseY = mouseY
--     , startAngle = angle
--     , touches = BoundedList.new 20 |> BoundedList.insert ( time, mouseY )
--     }
--
--
--
--
--
--
-- -- Model
--
--
-- setPicker : PickerId -> Picker -> Model -> Model
-- setPicker pickerId picker model =
--     case pickerId of
--         DayPicker ->
--             { model | dayPicker = picker }
--
--         HourPicker ->
--             { model | hourPicker = picker }
--
--         MinutePicker ->
--             { model | minutePicker = picker }
--
--
-- ---- VIEW ----
--
--
--
--
--
-- ---- MAIN ----
--
--
-- main : Program Never Model Msg
-- main =
--     Builder.program
--         { init = init
--         , update = update
--         , subscriptions = always Sub.none
--         , view = view
--         }
--
--
--
-- ---- HELPERS ----
--
--
--
--
-- computeNewSpeed : Speed -> Time -> Time -> Speed
-- computeNewSpeed speed currentTime lastTime =
--     speed * (0.99 ^ toFloat ((round (currentTime - lastTime)) % 17))
--
--
-- insignificantSpeed : Speed -> Bool
-- insignificantSpeed speed =
--     abs speed < 0.04
--
--
-- addToTouchesHistory : ( Time, MouseY ) -> TouchesHistory -> TouchesHistory
-- addToTouchesHistory touchCouple ({ touches } as touchesHistory) =
--     { touchesHistory | touches = BoundedList.insert touchCouple touches }
--
--
-- pickerMsgConstructor : PickerId -> PickerMsg -> Msg
-- pickerMsgConstructor pickerId =
--     case pickerId of
--         DayPicker ->
--             DayPickerMsg
--
--         HourPicker ->
--             HourPickerMsg
--
--         MinutePicker ->
--             MinutePickerMsg
--
--
-- getPicker : PickerId -> Model -> Picker
-- getPicker pickerId =
--     case pickerId of
--         DayPicker ->
--             .dayPicker
--
--         HourPicker ->
--             .hourPicker
--
--         MinutePicker ->
--             .minutePicker
--
--
--
--
--
--
