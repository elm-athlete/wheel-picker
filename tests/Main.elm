module Main exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import WheelPicker
import Test.Runner.Html


main : Test.Runner.Html.TestProgram
main =
    [ suite
    ]
        |> concat
        |> Test.Runner.Html.run


suite : Test
suite =
    describe "Test helper functions"
        [ test "speedToReachAFace modifies the original speed to perfectly reach a picker face" <|
            \_ ->
                Expect.equal
                    0.3174901573277509
                    (WheelPicker.speedToReachAFace
                        { previousTime = Nothing
                        , lastTime = 1
                        , direction = -1
                        , speed = 0.3
                        }
                        876
                        24
                    )
        ]
