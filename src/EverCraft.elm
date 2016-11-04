module EverCraft exposing (..)


hello : String -> String
hello thing =
    "hello " ++ thing


type alias Character =
    { name : String
    , alignment : Alignment
    , armorClass : ArmorClass
    , hitPoints : Int
    , strength : Int
    , dexterity : Int
    , constitution : Int
    , wisdom : Int
    , intelligence : Int
    , charisma : Int
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
    { name = "", alignment = Neutral, armorClass = 10, hitPoints = 5, strength = 10, dexterity = 10, constitution = 10, wisdom = 10, intelligence = 10, charisma = 10 }

damageDealtToDefender : AttackSuccess -> Character -> Int
damageDealtToDefender attackSuccess attacker =
    case attackSuccess of
        Critical ->
            max 1 ((1 + getModifier(attacker.strength)) * 2)
        Hit ->
            max 1 (1 + getModifier(attacker.strength))
        Miss ->
        0

evaluateAttackSuccess : DieRoll -> Int -> Character -> AttackSuccess
evaluateAttackSuccess dieRoll strengthModifier defender =
    if diceRoll == 20
        Critical
    else if diceRoll + strengthModifier >= defender.armorClass + getModifier(defender.dexterity)
        Hit
    else
        Miss

assignDamage : Int -> Character -> Int
assignDamage damageDealt defender =
    defender.hitPoints - damageDealt + getModifier(defender.constitution)


attack : DieRoll -> Character -> Character -> Character
attack diceRoll attacker defender =
{defender | hitPoints =
    evaluateAttackSuccess diceRoll (getModifier attacker.strength) defender |>
        damageDealtToDefender attacker


    damage = 1 + getModifier(attacker.strength)
    diceRoll = diceRoll + getModifier(attacker.strength)
    armorClassToBeat = defender.armorClass + getModifier(defender.dexterity)
    hitPoints = defender.hitPoints + getModifier(defender.hitPoints)
    if hitPoints == 0 then hitPoints = 1
    if diceRoll == 20 then

        { defender | hitPoints = defender.hitPoints - damage }
    else if (diceRoll >= defender.armorClass) then
        { defender | hitPoints = defender.hitPoints - damage }
    else
        defender


getModifier : Int -> Int
getModifier attributeValue =
    ((attributeValue // 2) - 5)
