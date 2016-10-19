module Tests exposing (..)

import Test exposing (..)
import Expect
import EverCraft exposing (..)


all : Test
all =
    describe "EverCraft"
        [ testCoreBasics ]


testCoreBasics =
    let
        character =
            { defaultCharacter | name = "name", alignment = Good, armorClass = 10, hitPoints = 5 }
    in
        describe "Character"
            [ test "has a name"
                <| \() ->
                    (character.name |> Expect.equal "name")
            , test "has an alignment"
                <| \() ->
                    (character.alignment |> Expect.equal Good)
            , test "can set armor class"
                <| \() ->
                    let
                        tougherCharacter =
                            { character | armorClass = 11 }
                    in
                        (tougherCharacter.armorClass |> Expect.equal 11)
            , test "can set hitPoints"
                <| \() ->
                    let
                        hurtCharacter =
                            { character | hitPoints = 3 }
                    in
                        (hurtCharacter.hitPoints |> Expect.equal 3)
            ]


testAttacks =
    describe "Attacking"
        [ test "check for hit"
            <| \() ->
                let
                    dieValue =
                        20

                    armorClass =
                        10
                in
                    (attack dieValue armorClass) |> Expect.equal True
        ]
