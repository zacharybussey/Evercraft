module Tests exposing (..)

import Test exposing (..)
import Expect
import EverCraft exposing (..)


all : Test
all =
    describe "EverCraft"
        [ testHello ]


testHello =
    let
        output =
            "hello world"
    in
        describe "hello"
            [ test "says hello world"
                <| \() ->
                    hello |> Expect.equal output
            ]
