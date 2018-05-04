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
                    ( 0.2918903903865285, 1, 936 )
                    (WheelPicker.speedToReachAFace 865 24 0.3)
        , test "same test with negative speed" <|
            \_ ->
                Expect.equal
                    ( 0.29597297173897485, -1, 792 )
                    (WheelPicker.speedToReachAFace 865 24 -0.3)
        , test "same test with direction different to original speed" <|
            \_ ->
                Expect.equal
                    ( 0.034641016151377546, -1, 864 )
                    (WheelPicker.speedToReachAFace 865 24 0.1)
        ]
