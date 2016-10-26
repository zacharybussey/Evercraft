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


defaultCharacter =
    { name = "", alignment = Neutral, armorClass = 10, hitPoints = 5, strength = 10, dexterity = 10, constitution = 10, wisdom = 10, intelligence = 10, charisma = 10 }


attack : DieRoll -> Character -> Character
attack diceRoll defender =
    if diceRoll == 20 then
        { defender | hitPoints = defender.hitPoints - 2 }
    else if (diceRoll >= defender.armorClass) then
        { defender | hitPoints = defender.hitPoints - 1 }
    else
        defender
