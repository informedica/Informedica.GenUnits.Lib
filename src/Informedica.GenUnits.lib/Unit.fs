namespace Informedica.GenUnits.Lib


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit =  

    open Informedica.GenUtils.Lib.BCL

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Multipliers =

        let one = 1N

        let kilo = 1000N
        let deci = 1N / 10N
        let centi = deci / 10N
        let milli = 1N / kilo
        let micro = milli / kilo                                                                                            
        let nano = micro / kilo

        let second = 1N
        let minute = 60N * second
        let hour = minute * minute
        let day = 24N * hour
        let week = 7N * day
        let month = 4N * week
        let year = 365N * day 

        let toBase m v  = v * m
        let toUnit m v  = v / m

    module MP = Multipliers

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name = 

        open Informedica.GenUtils.Lib.BCL

        type Name = Name of string

        type Message =
            | NameCannotBeNullOrWhiteString
            | NameCannotBeLongerThan30 of string

        exception NameException of Message

        let raiseExc m = m |> NameException |> raise

        let create succ fail s = 
            match s with
            | _ when s|> System.String.IsNullOrWhiteSpace -> 
                NameCannotBeNullOrWhiteString |> fail
            | _ when s.Length > 30 -> 
                s |> NameCannotBeLongerThan30 |> fail
            | _ -> 
                s |> String.trim |> Name |> succ

        let apply f (Name n) = n |> f

        let get = apply id

        let change f x = x |> apply f |> create

        let eqs s n = n |> get |> String.equalsCapInsens s

        let toLower = get >> String.toLower >> Name

        let capitalize = get >> String.capitalize >> Name

        let toString (Name n) = n

    module N = Name

    type Message =
        | NameMessage of N.Message
        | MultiplierNotPositive of BigRational
        | UnitNotFound of string
        | CannotConvert of N.Name * N.Name

    exception UnitException of Message

    let raiseExc m = m |> UnitException |> raise

    [<Literal>]
    let message = "Could not find "
        
    let unitNotFound s = message + "unit: " + s
    let weightNotFound s = message + "weight unit: " + s
    let massNotFound s = message + "mass unit: " + s
    let timeNotFound s = message + "time unit: " + s
    let distanceNotFound s = message + "distance unit: " + s
    let adjustNotFound s = message + "adjust unit: " + s

    type Unit = 
        { 
            Group: N.Name
            Name: N.Name * N.Name list
            Abbreviation: N.Name * N.Name list
            Multiplier: BigRational
        }

    let create succ fail g n a m =
        if m <= 0N then m |> MultiplierNotPositive |> fail
        else 
            { 
                Group = g |> N.toLower |> N.capitalize
                Name = n, []
                Abbreviation = a, []
                Multiplier = m 
            }
            |> succ

    let applyToUnit f (u: Unit) = u |> f

    let getMultiplier   = applyToUnit (fun u -> u.Multiplier)

    let getGroupName    = applyToUnit (fun u -> u.Group)

    let getName         = applyToUnit (fun u -> u.Name)

    let getAbbreviation = applyToUnit (fun u -> u.Abbreviation)

    let convert f u v = v |> f (u |> getMultiplier)

    let toUnit  = convert MP.toUnit 

    let toBase  = convert MP.toBase

    let unitToUnit succ fail u1 u2 = 
        let gr1, gr2 = u1 |> getGroupName, u2 |> getGroupName
        if gr1 = gr2 then u1 |> toBase >> (u2 |> toUnit) |> succ
        else (gr1, gr2) |> CannotConvert |> fail

    let eqGroup u1 u2 = let g = u1|> getGroupName in u2 |> getGroupName = g

    module Units =

        module C = Constants
        module N = Name

        let name = N.create id (fun m -> m |> N.raiseExc)

        let create' g n a m =
            let g' = name g
            let n' = name n
            let a' = name a
            create id (fun m -> m |> raiseExc) g' n' a' m

        [<Literal>]
        let countGroup = C.countGroup
        let createCount = create' countGroup
        let count =  createCount "Times" "X" MP.one  
        let countUnits = [count] 

        [<Literal>]
        let massGroup = C.massGroup
        let createMass = create' massGroup
        let kiloGram  = createMass "KiloGram"  "kg"    MP.kilo
        let gram      = createMass "Gram"      "g"     MP.one
        let milliGram = createMass "MilliGram" "mg"    MP.milli
        let microGram = createMass "MicroGram" "mcg"   MP.micro
        let nanoGram  = createMass "NanoGram"  "nanog" MP.nano
        let massUnits = [kiloGram;gram;milliGram;microGram;nanoGram]

        [<Literal>]
        let molarGroup = C.molarGroup
        let createMolar = create' molarGroup
        let mol      = createMass "Mol"      "mol"  MP.one  
        let milliMol = createMass "MilliMol" "mmol" MP.milli  
        let molarUnits = [mol;milliMol] 

        [<Literal>]
        let weightGroup = C.weightGroup
        let createWeight = create' weightGroup
        let weightKg   = createWeight "KiloGram" "kg" MP.kilo 
        let weightGram = createWeight "Gram"     "g"  MP.one
        let weightUnits = [weightKg;weightGram] 

        [<Literal>]
        let bsaGroup = C.bsaGroup
        let createBSA = create' bsaGroup
        let bsa = createBSA "SquareMeter" "m^s" MP.kilo 
        let bsaUnits = [bsa]

        [<Literal>]
        let volumeGroup = C.volumeGroup
        let createVolume = create' volumeGroup
        let liter      = createVolume "Liter"      "l"   MP.one
        let deciLiter  = createVolume "DeciLiter"  "dl"  MP.deci 
        let milliLiter = createVolume "MilliLiter" "ml"  MP.milli 
        let microLiter = createVolume "MicroLiter" "mcl" MP.micro 
        let volumeUnits = [liter;deciLiter;milliLiter;microLiter]

        [<Literal>]
        let timeGroup = C.timeGroup
        let createTime = create' timeGroup
        let second = createTime "Second" "sec"  MP.one 
        let minute = createTime "Minute" "min"  MP.minute
        let hour   = createTime "Hour"   "hr"   MP.hour
        let day    = createTime "Day"    "day"  MP.day
        let week   = createTime "Week"   "week" MP.week
        let month  = createTime "Month"  "mo"   MP.month 
        let year   = createTime "Year"   "yr"   MP.year
        let timeUnits = [second;minute;hour;day;week;month;year] 

        [<Literal>]
        let distanceGroup = C.distanceGroup
        let createDistance = create' distanceGroup
        let meter      = createDistance "Meter"      "m"  MP.one 
        let centimeter = createDistance "CentiMeter" "cm" MP.centi 
        let distanceUnits = [meter;centimeter]

        let units = [countUnits;massUnits;molarUnits;weightUnits;bsaUnits;volumeUnits;timeUnits;distanceUnits]

        let isGroup gr u = u |> getGroupName |> N.eqs gr
        let isTime = isGroup timeGroup
        let isVolume = isGroup volumeGroup
        let isAdjust u = u |> isGroup weightGroup || (u |> isGroup bsaGroup)

        let hasName s u = 
            let eqs = N.eqs s
            let (n, ns) = getName u
            let (a, aa) = getAbbreviation u
            n |> eqs || ns |> List.exists eqs ||
            a |> eqs || aa |> List.exists eqs

        let find succ fail us g s = 
            let u =
                us 
                |> List.collect id
                |> List.tryFind (fun u -> u |> hasName s && u |> isGroup g)

            match u with 
            | Some u -> u |> succ
            | None   -> s |> UnitNotFound |> fail

        let find' us succ fail = find succ fail us
        let weightFromString succ fail   = find' [weightUnits] succ fail
        let distanceFromString succ fail = find' [distanceUnits] succ fail
        let adjustFromString succ fail   = find' [weightUnits;bsaUnits] succ fail
        let timeFromString succ fail     = find' [timeUnits] succ fail

        /// Create a unit from string `s` with
        /// group `g`.
        let fromString s g =
            match s |> find Some (fun _ -> None) units g with
            | Some(u) -> u |> Some
            | None -> 
                let name = N.create Some (fun _ -> None)
                let g = name (s |> String.capitalize)
                let n = name (s |> String.toLower)
                let a = name (s |> String.toLower) 
                match g, n, a with
                | Some (g'), Some (n'), Some (a') -> 
                    create Some (fun _ -> None) g' n' a' MP.one
                | _ -> None

         