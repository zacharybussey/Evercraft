module EverCraft exposing (..)


hello : String -> String
hello thing =
    "hello " ++ thing


type alias Character =
    { name : String
    , alignment : Alignment
    , armorClass : ArmorClass
    , hitPoints : Int
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
    { name = "", alignment = Neutral, armorClass = 10, hitPoints = 5 }


attack : DieRoll -> ArmorClass -> Bool
attack diceRoll armorClass =
    diceRoll >= armorClass
