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
    , level : Int
    , attackBonus : Int
    , class : Class
    }

type Class
    = NotSet
    | Fighter
    | Rogue
    | Monk
    | Paladin

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
    { name = "", alignment = Neutral, armorClass = 10, hitPoints = 5, maxHitPoints = 5, strength = 10, dexterity = 10, constitution = 10, wisdom = 10, intelligence = 10, charisma = 10, experience = 0, level = 1, attackBonus = 0, class = NotSet }


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


checkLevel : Character -> Character
checkLevel ({ level, experience, hitPoints, maxHitPoints } as attacker) =
    let
        hitPointsToAdd = max (5 + (getModifier attacker.constitution)) 1
        experienceLevel =
            experience // 1000 + 1
        newAtackBonus = getNewAttackBonus experienceLevel attacker.attackBonus
    in
        if level < experienceLevel then
            { attacker | level = experienceLevel, hitPoints = hitPoints + hitPointsToAdd, maxHitPoints = maxHitPoints + hitPointsToAdd, attackBonus = newAtackBonus }
        else
            attacker

getNewAttackBonus : Int -> Int -> Int
getNewAttackBonus level oldAttackBonus =
    let
        addAttackBonus = rem level 2 == 0
    in
        if addAttackBonus then
            oldAttackBonus + 1
        else
            oldAttackBonus

experienceize : AttackSuccess -> Character -> Character
experienceize attackSuccess attacker =
    let
        gainedXP =
            if attackSuccess /= Miss then
                10
            else
                0
    in
        checkLevel { attacker | experience = attacker.experience + gainedXP }


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
        ( attacker |> experienceize attackSuccess
        , assignDamage attacker defender attackSuccess
        )
