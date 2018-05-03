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


approachSpeed : Float
approachSpeed =
    0.05


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
    -- ( ( t-1, t ), deg/ms )
    ( ( Maybe Time, Time ), Speed )


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
                |> updateAngle
            , Cmd.none
            )

        StopTouching ->
            ( WheelPicker picker
                |> addNewTouch ( currentTime, mouseY )
                |> setStateWhenStopTouching
                |> updateAngle
            , Cmd.none
            )


updateNewFrame : Time -> WheelPicker -> ( WheelPicker, Cmd Msg )
updateNewFrame currentTime (WheelPicker picker) =
    (WheelPicker picker
        |> setStateFromNewFrame currentTime
        |> updateAngle
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
    in
        if picker.angle < 0 then
            WheelPicker picker
                |> setAngle 0
        else if picker.angle > maxAngle then
            WheelPicker picker
                |> setAngle maxAngle
        else
            WheelPicker picker


setStateWhenStopTouching : WheelPicker -> WheelPicker
setStateWhenStopTouching (WheelPicker picker) =
    case picker.state of
        Held touchesHistory ->
            newStateFromTouchesHistory picker.radiusOut touchesHistory
                |> flip setState (WheelPicker picker)

        _ ->
            WheelPicker picker


setStateFromNewFrame : Time -> WheelPicker -> WheelPicker
setStateFromNewFrame currentTime (WheelPicker picker) =
    case picker.state of
        Free speedState ->
            speedStateFromNewFrame currentTime speedState
                |> speedStateToState
                |> flip setState (WheelPicker picker)

        _ ->
            WheelPicker picker


speedStateFromNewFrame : Time -> SpeedState -> SpeedState
speedStateFromNewFrame currentTime ( ( _, previousTime ), previousSpeed ) =
    ( ( Just previousTime, currentTime )
    , previousSpeed - friction * (currentTime - previousTime)
    )


newStateFromTouchesHistory : Int -> TouchesHistory -> State
newStateFromTouchesHistory pickerRadius touchesHistory =
    let
        calculateSpeedState ( ( lastTime, lastMouseY ), ( firstTime, firstMouseY ) ) =
            ( ( Nothing, lastTime )
            , if (lastTime - firstTime) > 500 then
                0
              else
                (degPerPx pickerRadius) * (firstMouseY - lastMouseY) / (lastTime - firstTime)
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
        touchesHistory.touches
            |> touchesSample
            |> calculateSpeedState
            |> speedStateToState


speedStateToState : SpeedState -> State
speedStateToState (( _, speed ) as speedState) =
    if abs speed < approachSpeed then
        Stopped
    else
        Free speedState


updateAngle : WheelPicker -> WheelPicker
updateAngle (WheelPicker picker) =
    (case picker.state of
        Free speedState ->
            WheelPicker picker
                |> setAngle (angleFromSpeedState picker.angle speedState)

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


angleFromSpeedState : Angle -> SpeedState -> Angle
angleFromSpeedState currentAngle ( ( maybePreviousTime, lastTime ), speed ) =
    case maybePreviousTime of
        Nothing ->
            currentAngle

        Just previousTime ->
            currentAngle + speed * (lastTime - previousTime)


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
