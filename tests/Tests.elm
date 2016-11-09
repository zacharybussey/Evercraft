module Tests exposing (..)

import Test exposing (..)
import Expect
import EverCraft exposing (..)


all : Test
all =
    describe "EverCraft"
        [ testCoreBasics
        , testAttacks
        ]


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
            , test "can set strength"
                <| \() ->
                    let
                        strongCharacter =
                            { character | strength = 13 }
                    in
                        (strongCharacter.strength |> Expect.equal 13)
            , test "has default attributes"
                <| \() ->
                    let
                        expectedDefaultCharacter =
                            { name = "", alignment = Neutral, armorClass = 10, hitPoints = 5, maxHitPoints = 5, strength = 10, dexterity = 10, constitution = 10, wisdom = 10, intelligence = 10, charisma = 10 }
                    in
                        (defaultCharacter |> Expect.equal expectedDefaultCharacter)
            , test "modifiers are calculated correctly"
                <| \() ->
                    let
                        inputs =
                            [1..20]

                        outputs =
                            [ -5, -4, -4, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5 ]
                    in
                        ((List.map getModifier inputs) |> Expect.equal outputs)
            , test "constitution modifier affects max hitpoints"
                <| \() ->
                    let
                        character =
                            { defaultCharacter | constitution = 16 }
                                |> applyModifiers
                    in
                        character |> Expect.equal { defaultCharacter | constitution = 16, maxHitPoints = 8 }
            ]


testAttacks =
    let
        defender =
            defaultCharacter

        hurtDefender =
            { defaultCharacter | hitPoints = 4 }

        criticalDefender =
            { defaultCharacter | hitPoints = 3 }

        deadDefender =
            { defaultCharacter | hitPoints = -3 }

        nimbleDefender =
            { defaultCharacter | dexterity = 16 }

        strongAttacker =
            { defaultCharacter | strength = 16 }

        heartyDefender =
            { defaultCharacter | constitution = 16 }

        clumsyDefender =
            { defaultCharacter | dexterity = 1 }

        weakAttacker =
            { defaultCharacter | strength = 1 }

        infirmDefender =
            { defaultCharacter | constitution = 1 }
    in
        describe "Attacking"
            [ test "check for hit"
                <| \() ->
                    (attack 11 defaultCharacter defender |> Expect.equal hurtDefender)
            , test "check for miss"
                <| \() ->
                    (attack 9 defaultCharacter defender) |> Expect.equal defender
            , test "check for equal"
                <| \() ->
                    (attack 10 defaultCharacter defender) |> Expect.equal hurtDefender
            , test "check for critical"
                <| \() ->
                    (attack 20 defaultCharacter defender) |> Expect.equal criticalDefender
            , test "check for hit with strong attacker"
                <| \() ->
                    (attack 20 strongAttacker defender) |> Expect.equal deadDefender
            ]
