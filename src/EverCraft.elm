module EverCraft exposing (..)


hello : String -> String
hello thing =
    "hello " ++ thing


type alias Character =
    { name : String
    , alignment : Alignment
    , armorClass : ArmorClass
    , hitPoints : Int
    , maxHitPoints : Int
    , strength : Int
    , dexterity : Int
    , constitution : Int
    , wisdom : Int
    , intelligence : Int
    , charisma : Int
    , experience : Int
    }


type alias ArmorClass =
    Int


type alias DieRoll =
    Int


type Alignment
    = Good
    | Evil
    | Neutral


type AttackSuccess
    = Critical
    | Hit
    | Miss


defaultCharacter =
    { name = "", alignment = Neutral, armorClass = 10, hitPoints = 5, maxHitPoints = 5, strength = 10, dexterity = 10, constitution = 10, wisdom = 10, intelligence = 10, charisma = 10, experience = 0 }


applyModifiers : Character -> Character
applyModifiers ({ constitution } as character) =
    { character | maxHitPoints = character.maxHitPoints + getModifier constitution }


getModifier : Int -> Int
getModifier attributeValue =
    ((attributeValue // 2) - 5)


damageDealtToDefender : Character -> AttackSuccess -> Int
damageDealtToDefender attacker attackSuccess =
    case attackSuccess of
        Critical ->
            max 1 ((1 + getModifier (attacker.strength)) * 2)

        Hit ->
            max 1 (1 + getModifier (attacker.strength))

        Miss ->
            0


evaluateAttackSuccess : DieRoll -> Int -> Character -> AttackSuccess
evaluateAttackSuccess dieRoll strengthModifier defender =
    if dieRoll == 20 then
        Critical
    else if dieRoll + strengthModifier >= defender.armorClass + getModifier (defender.dexterity) then
        Hit
    else
        Miss


calcDamageDelt : Character -> Int -> Int
calcDamageDelt defender damageDealt =
    defender.hitPoints - damageDealt + getModifier (defender.constitution)


experienceize : Character -> AttackSuccess -> Character
experienceize attacker attackSuccess =
    let
        gainedXP =
            if attackSuccess /= Miss then
                10
            else
                0
    in
        { attacker | experience = attacker.experience + gainedXP }


assignDamage : Character -> Character -> AttackSuccess -> Character
assignDamage attacker defender attackSuccess =
    { defender
        | hitPoints =
            attackSuccess
                |> damageDealtToDefender attacker
                |> calcDamageDelt defender
    }


attack : DieRoll -> Character -> Character -> ( Character, Character )
attack diceRoll attacker defender =
    let
        attackSuccess =
            evaluateAttackSuccess diceRoll (getModifier attacker.strength) defender
    in
        ( experienceize attacker attackSuccess
        , assignDamage attacker defender attackSuccess
        )
