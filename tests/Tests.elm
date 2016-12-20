module Tests exposing (..)

import Test exposing (..)
import Expect
import EverCraft exposing (..)


all : Test
all =
    describe "EverCraft"
        [ testCoreBasics
        , testAttacks
        , testFighter
        ]


testCoreBasics =
    let
        character =
            { defaultCharacter | name = "name", alignment = Good, armorClass = 10, hitPoints = 5 }
    in
        describe "Character"
            [ test "has a name" <|
                \() ->
                    (character.name |> Expect.equal "name")
            , test "has an alignment" <|
                \() ->
                    (character.alignment |> Expect.equal Good)
            , test "can set armor class" <|
                \() ->
                    let
                        tougherCharacter =
                            { character | armorClass = 11 }
                    in
                        (tougherCharacter.armorClass |> Expect.equal 11)
            , test "can set hitPoints" <|
                \() ->
                    let
                        hurtCharacter =
                            { character | hitPoints = 3 }
                    in
                        (hurtCharacter.hitPoints |> Expect.equal 3)
            , test "can set strength" <|
                \() ->
                    let
                        strongCharacter =
                            { character | strength = 13 }
                    in
                        (strongCharacter.strength |> Expect.equal 13)
            , test "modifiers are calculated correctly" <|
                \() ->
                    let
                        inputs =
                            [1..20]

                        outputs =
                            [ -5, -4, -4, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5 ]
                    in
                        ((List.map getModifier inputs) |> Expect.equal outputs)
            , test "constitution modifier affects max hitpoints" <|
                \() ->
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

        experiencedAttacker =
            { defaultCharacter | experience = 10 }

        strongExperiencedAttacker =
            { defaultCharacter | experience = 10, strength = 16 }

        aboutToLevelToEvenLevelCharacter =
            { defaultCharacter | experience = 990, constitution = 12 }

        aboutToLevelToOddLevelCharacter =
            { defaultCharacter | experience = 1990, constitution = 8 }
    in
        describe "Attacking"
            [ test "check for hit" <|
                \() ->
                    (assignDamage defaultCharacter defender Hit) |> Expect.equal hurtDefender
            , test "check for miss" <|
                \() ->
                    (assignDamage defaultCharacter defender Miss) |> Expect.equal defender
            , test "check for critical" <|
                \() ->
                    (assignDamage defaultCharacter defender Critical) |> Expect.equal criticalDefender
            , test "check for hit with strong attacker" <|
                \() ->
                    (assignDamage strongAttacker defender Critical) |> Expect.equal deadDefender
            , test "experienceize hit" <|
                \() ->
                    (experienceize Hit defaultCharacter) |> Expect.equal experiencedAttacker
            , test "experienceize miss" <|
                \() ->
                    (experienceize Miss defaultCharacter) |> Expect.equal defaultCharacter
            , test "experienceize critical" <|
                \() ->
                    (experienceize Critical defaultCharacter) |> Expect.equal experiencedAttacker
            , test "experienceize to even level" <|
                \() ->
                    (experienceize Hit aboutToLevelToEvenLevelCharacter) |> Expect.equal { aboutToLevelToEvenLevelCharacter | experience = 1000, maxHitPoints = 11, hitPoints = 11, level = 2, attackBonus = 1 }
            , test "experienceze to odd level" <|
                \() ->
                    (experienceize Hit aboutToLevelToOddLevelCharacter) |> Expect.equal { aboutToLevelToOddLevelCharacter | experience = 2000, maxHitPoints = 9, hitPoints = 9, level = 3 }
            ]


testFighter =
    let
        fighter =
            makeNewCharacter Fighter

        aboutToLevelFighter =
            { fighter | experience = 1000 }

        leveledFighter =
            { aboutToLevelFighter | attackBonus = 1, hitPoints = 15, maxHitPoints = 15, level = 2 }
    in
        describe "Fighter class attributes"
            [ test "attack/hp bonus" <|
                \() ->
                    (checkLevel aboutToLevelFighter) |> Expect.equal leveledFighter
            ]
