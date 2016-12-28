module EverCraft exposing (..)


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
    , classFeatures : ClassFeatures
    }


type Class
    = NotSet
    | Fighter
    | Rogue
    | Monk
    | Paladin


type alias ClassFeatures =
    { getNewAttackBonus : Int -> Int -> Int
    , getHitpointBonus : Int -> Int
    , getCriticalHitDamage : Int -> Int
    , getAttackAttribute : Blah
    }


type Blah
    = Blah (Character -> Int)


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


defaultClassFeatures =
    { getNewAttackBonus = defaultGetNewAttackBonus
    , getHitpointBonus = defaultGetHitpointBonus
    , getCriticalHitDamage = getCriticalHitDamageDefault
    , getAttackAttribute = Blah (\character -> character.strength)
    }


fighterClassFeatures =
    { defaultClassFeatures
        | getNewAttackBonus = getNewAttackBonusFighter
        , getHitpointBonus = getHitpointBonusFighter
    }


rogueClassFeatures =
    { defaultClassFeatures
        | getCriticalHitDamage = getCriticalHitDamageRogue
        , getAttackAttribute = Blah (\character -> character.dexterity)
    }


defaultCharacter =
    { name = "", alignment = Neutral, armorClass = 10, hitPoints = 5, maxHitPoints = 5, strength = 10, dexterity = 10, constitution = 10, wisdom = 10, intelligence = 10, charisma = 10, experience = 0, level = 1, attackBonus = 0, class = NotSet, classFeatures = defaultClassFeatures }


makeNewCharacter : Class -> Character
makeNewCharacter class =
    case class of
        Fighter ->
            { defaultCharacter | classFeatures = fighterClassFeatures }

        Rogue ->
            { defaultCharacter | classFeatures = rogueClassFeatures }

        _ ->
            defaultCharacter


applyModifiers : Character -> Character
applyModifiers ({ constitution } as character) =
    { character | maxHitPoints = character.maxHitPoints + getModifier constitution }


getModifier : Int -> Int
getModifier attributeValue =
    ((attributeValue // 2) - 5)


damageDealtToDefender : Character -> AttackSuccess -> Int
damageDealtToDefender attacker attackSuccess =
    let
        attackAttribute =
            case attacker.classFeatures.getAttackAttribute of
                Blah fn ->
                    fn attacker
    in
        case attackSuccess of
            Critical ->
                attacker.classFeatures.getCriticalHitDamage attackAttribute

            Hit ->
                max 1 (1 + getModifier (attackAttribute))

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
checkLevel ({ level, experience, hitPoints, maxHitPoints, class } as attacker) =
    let
        hitPointsToAdd =
            attacker.classFeatures.getHitpointBonus attacker.constitution

        experienceLevel =
            experience // 1000 + 1

        newAttackBonus =
            attacker.classFeatures.getNewAttackBonus experienceLevel attacker.attackBonus
    in
        if level < experienceLevel then
            { attacker | level = experienceLevel, hitPoints = hitPoints + hitPointsToAdd, maxHitPoints = maxHitPoints + hitPointsToAdd, attackBonus = newAttackBonus }
        else
            attacker


defaultGetNewAttackBonus : Int -> Int -> Int
defaultGetNewAttackBonus level oldAttackBonus =
    let
        addAttackBonus =
            rem level 2 == 0
    in
        if addAttackBonus then
            oldAttackBonus + 1
        else
            oldAttackBonus


getNewAttackBonusFighter : Int -> Int -> Int
getNewAttackBonusFighter level oldAttackBonus =
    oldAttackBonus + 1


defaultGetHitpointBonus : Int -> Int
defaultGetHitpointBonus constitution =
    max (5 + (getModifier constitution)) 1


getHitpointBonusFighter : Int -> Int
getHitpointBonusFighter constitution =
    max (10 + (getModifier constitution)) 1


getCriticalHitDamageDefault : Int -> Int
getCriticalHitDamageDefault strength =
    max 1 ((1 + getModifier (strength)) * 2)


getCriticalHitDamageRogue : Int -> Int
getCriticalHitDamageRogue strength =
    max 3 ((1 + getModifier (strength)) * 3)


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
