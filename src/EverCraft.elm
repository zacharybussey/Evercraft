module EverCraft exposing (..)


hello : String -> String
hello thing =
    "hello " ++ thing


type alias Character =
    { name : String
    , alignment : Alignment
    , armorClass : Int
    , hitPoints : Int
    }


type Alignment
    = Good
    | Evil
    | Neutral


defaultCharacter =
    { name = "", alignment = Neutral, armorClass = 10, hitPoints = 5 }


attack : Int -> Int -> Bool
attack diceRoll armorClass =
    diceRoll >= armorClass
