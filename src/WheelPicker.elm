module WheelPicker
    exposing
        ( WheelPicker
        , Msg(..)
        , GetTouchMsg(..)
        , update
        , view
        , defaultWheelPicker
        , getSelect
        , isAnimationFrameNeeded
        , speedToReachAFace
        )

import BodyBuilder as Builder exposing (Node)
import BodyBuilder.Attributes as Attributes
import Elegant exposing (px, vh, percent, Style, deg)
import Style
import Box
import Block
import Color
import Transform
import Position
import Typography
import Time
import Task
import Touch exposing (Coordinates)
import BoundedList exposing (BoundedList)
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


friction : Float
friction =
    -- deg/ms^2
    0.0006



---- MODEL ----
{- Sub-Models -}


type alias DataList =
    List String


type alias Angle =
    Float


type State
    = Stopped
    | Free SpeedState
    | Held TouchesHistory
    | Approach


type alias Speed =
    -- deg/ms
    Float


type alias SpeedState =
    { startTime : Time
    , finalTime : Time
    , startAngle : Angle
    , finalAngle : Angle
    , startSpeed : Speed
    , direction : Float
    }


type alias MouseY =
    Float


initTouchesHistory : Time -> MouseY -> Angle -> TouchesHistory
initTouchesHistory time mouseY angle =
    { startMouseY = mouseY
    , startAngle = angle
    , touches = BoundedList.new touchesHistoryLength |> BoundedList.insert ( time, mouseY )
    }


addNewTouch : ( Time, MouseY ) -> WheelPicker -> WheelPicker
addNewTouch touch (WheelPicker picker) =
    case picker.state of
        Held touchesHistory ->
            { touchesHistory | touches = BoundedList.insert touch touchesHistory.touches }
                |> Held
                |> flip setState (WheelPicker picker)

        _ ->
            WheelPicker picker


type alias TouchesHistory =
    { startMouseY : MouseY
    , startAngle : Angle
    , touches : BoundedList ( Time, MouseY )
    }



{- Model -}


setAngle : Angle -> WheelPicker -> WheelPicker
setAngle angle ((WheelPicker picker) as wheelPicker) =
    WheelPicker { picker | angle = angle }


setState : State -> WheelPicker -> WheelPicker
setState state (WheelPicker picker) =
    WheelPicker { picker | state = state }


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
        , state = Stopped
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
        , state : State
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
                |> setState (Held (initTouchesHistory currentTime mouseY picker.angle))
            )
                ! []

        HoldTouching ->
            ( WheelPicker picker
                |> addNewTouch ( currentTime, mouseY )
                |> updateAngle currentTime
            , Cmd.none
            )

        StopTouching ->
            ( WheelPicker picker
                |> setStateWhenStopTouching
            , Cmd.none
            )


updateNewFrame : Time -> WheelPicker -> ( WheelPicker, Cmd Msg )
updateNewFrame currentTime (WheelPicker picker) =
    (WheelPicker picker
        |> updateAngle currentTime
        |> setStateFromNewFrame currentTime
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
                |> setSelect (resolveSelect newPicker)
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
            ((picker.angle + 180 - (toFloat faceIndex) * (angleBetweenFaces picker.faces)) / 360 |> floor) * picker.faces + faceIndex

        selectionToString ( _, date, _ ) =
            String.join " " <|
                [ toString (Date.day date)
                , toString (Date.month date)
                , toString (Date.year date)
                ]

        faceColorOpacity faceIndex =
            if ((elementsToDrop faceIndex |> toFloat) - picker.angle) >= 80 then
                0
            else
                (80 - ((elementsToDrop faceIndex |> toFloat) * (angleBetweenFaces picker.faces) - picker.angle |> abs)) / 80

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
                        , (if (elementsToDrop faceIndex - picker.select) == 0 then
                            Box.textColor <| Color.rgb 0 0 0
                           else
                            Box.textColor <| Color.rgba 180 180 180 (faceColorOpacity faceIndex)
                          )
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
        List.map pickerViewFace (List.range 0 (picker.faces - 1))


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
                        [ Transform.perspective (px 500)
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

        stopIfFree =
            case picker.state of
                Free _ ->
                    Stopped

                _ ->
                    picker.state
    in
        if picker.angle < 0 then
            WheelPicker picker
                |> setAngle 0
                |> setState stopIfFree
        else if picker.angle > maxAngle then
            WheelPicker picker
                |> setAngle maxAngle
                |> setState stopIfFree
        else
            WheelPicker picker


setStateWhenStopTouching : WheelPicker -> WheelPicker
setStateWhenStopTouching (WheelPicker picker) =
    case picker.state of
        Held touchesHistory ->
            newStateFromTouchesHistory (WheelPicker picker) touchesHistory
                |> flip setState (WheelPicker picker)

        _ ->
            WheelPicker picker


setStateFromNewFrame : Time -> WheelPicker -> WheelPicker
setStateFromNewFrame currentTime (WheelPicker picker) =
    case picker.state of
        Free speedState ->
            if currentTime >= speedState.finalTime then
                WheelPicker picker
                    |> setAngle speedState.finalAngle
                    |> setState Stopped
            else
                WheelPicker picker
                    |> setState (Free speedState)

        _ ->
            WheelPicker picker


speedToReachAFace : Angle -> Angle -> Speed -> ( Speed, Float, Angle )
speedToReachAFace pickerAngle pickerAngleBetweenFaces speed =
    let
        finalAngle =
            ((speed ^ 2) / (2 * friction))
                |> (*) (speed / (abs speed))
                |> (+) pickerAngle
                |> flip (/) pickerAngleBetweenFaces
                |> round
                |> toFloat
                |> (*) pickerAngleBetweenFaces

        direction =
            if finalAngle >= pickerAngle then
                1
            else
                -1
    in
        ( finalAngle
            |> (-) pickerAngle
            |> abs
            |> (*) (2 * friction)
            |> sqrt
        , direction
        , finalAngle
        )


newStateFromTouchesHistory : WheelPicker -> TouchesHistory -> State
newStateFromTouchesHistory (WheelPicker picker) touchesHistory =
    let
        speedToSpeedState ( speed, direction, finalAngle ) =
            { startTime = currentTime
            , finalTime = currentTime + speed / friction
            , startAngle = picker.angle
            , finalAngle = finalAngle
            , startSpeed = speed
            , direction = direction
            }

        touchesSampleToSpeed ( ( lastTime, lastMouseY ), ( firstTime, firstMouseY ) ) =
            (degPerPx picker.radiusOut) * (firstMouseY - lastMouseY) / (lastTime - firstTime)

        currentTime =
            touchesHistory.touches
                |> BoundedList.head
                |> Maybe.withDefault ( 0, 0 )
                |> Tuple.first

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
        touchesHistory.touches
            |> touchesSample
            |> touchesSampleToSpeed
            |> speedToReachAFace picker.angle (angleBetweenFaces picker.faces)
            |> speedToSpeedState
            |> Free


updateAngle : Time -> WheelPicker -> WheelPicker
updateAngle currentTime (WheelPicker picker) =
    (case picker.state of
        Free speedState ->
            WheelPicker picker
                |> setAngle (angleFromSpeedState currentTime speedState)

        Held touchesHistory ->
            WheelPicker picker
                |> setAngle (angleFromTouchesHistory picker.radiusOut picker.angle touchesHistory)

        Approach ->
            WheelPicker picker

        Stopped ->
            WheelPicker picker
    )
        |> applyLimitAngles


angleFromTouchesHistory : Int -> Angle -> TouchesHistory -> Angle
angleFromTouchesHistory pickerRadius currentAngle touchesHistory =
    touchesHistory.touches
        |> BoundedList.content
        |> List.take 2
        |> (\list ->
                case list of
                    ( _, mouseY ) :: xs ->
                        touchesHistory.startAngle + (degPerPx pickerRadius) * (touchesHistory.startMouseY - mouseY)

                    _ ->
                        currentAngle
           )


angleFromSpeedState : Time -> SpeedState -> Angle
angleFromSpeedState currentTime speedState =
    let
        deltaTime =
            currentTime - speedState.startTime
    in
        speedState.startAngle - speedState.direction * (0.5 * friction * deltaTime ^ 2 - speedState.startSpeed * deltaTime)


resolveSelect : WheelPicker -> Int
resolveSelect (WheelPicker picker) =
    picker.angle / (angleBetweenFaces picker.faces) |> round


isAnimationFrameNeeded : WheelPicker -> Bool
isAnimationFrameNeeded (WheelPicker picker) =
    case picker.state of
        Held _ ->
            False

        Stopped ->
            False

        _ ->
            True
