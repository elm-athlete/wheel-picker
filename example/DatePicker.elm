module DatePicker exposing (main)

import BodyBuilder as Builder exposing (Node)
import WheelPicker
import Date


---- INIT ----


type alias Model =
    { date : Maybe Date.Date
    , dayPicker : Picker.Picker
    , hourPicker : Picker.Picker
    , minutePicker : Picker.Picker
    }


initModel : Model
initModel =
    { date = Nothing
    , dayPicker = initDayPicker
    , hourPicker = initHourPicker
    , minutePicker = initMinutePicker
    }


init : ( Model, Cmd Msg )
init =
    initModel ! []



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- VIEW ----


view : Model -> Node Msg
view model =
    Builder.div [] []



---- MAIN ----


main : Program Never Model Msg
main =
    Builder.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
