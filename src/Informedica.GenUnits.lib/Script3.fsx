#I "./Scripts"
#load "load-project-release.fsx"

#time

open Microsoft.FSharp.Reflection

module StringBCL =

    let toString s = s.ToString()

    let empty (s: string) = System.String.IsNullOrWhiteSpace(s)

    let notEmpty = empty >> not

    let toUpper (s: string) = s.ToUpper()

    let toLower (s: string) = s.ToLower()

    let length (s: string) = s.Length

    let substring start length (s: string) = s.Substring(start, length)

    let trim (s: string) = s.Trim()

    /// Get the first character of a string
    /// as a string
    let firstStringChar = substring 0 1

    /// Return the rest of a string as a string
    let restString s = substring 1 ((s |> length) - 1) s

    let firstToUpper = firstStringChar >> toUpper

    let capitalize s = (s |> firstToUpper) + (s |> restString |> toLower)

    let letters = ['a'..'z'] @ ['A'..'Z'] |> List.map toString

    let isLetter s = List.exists (fun s' -> s' = s) letters

    let equalsCapInsens s1 s2 = s1 |> toLower |> trim = (s2 |> toLower |> trim) 


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


module TestMultipliers =
    open Swensen.Unquote
    module MP = Multipliers

    let ``Converting value to base and back to unit equals the value`` =
        fun () -> 
            let c = MP.toBase MP.kilo >> MP.toUnit MP.kilo 
            test<@ 10N |> c = 10N @>

    let run() =
        [
            ``Converting value to base and back to unit equals the value``
        ] |> List.iter (fun t -> t())

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit =  

    module MP = Multipliers

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name = 

        module SBCL = StringBCL

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
                s |> SBCL.trim |> Name |> succ

        let apply f (Name n) = n |> f

        let get = apply id

        let change f x = x |> apply f |> create

        let eqs s n = n |> get |> SBCL.equalsCapInsens s

        let toLower = get >> SBCL.toLower >> Name

        let capitalize = get >> SBCL.capitalize >> Name

        module NameTests =
            
            open Swensen.Unquote

            // Name cannot be created with an empty or null string
            let succ _ = false
            let fail _ = true
            test <@ create succ fail ""  @>
            test <@ create succ fail null @>

            // Name should be at least one character long
            // and smaller than 30 characters
            test <@ create (fun _ -> true) (fun _ -> false) "1" @>
            test <@ "s" |> String.replicate 31 |> create (fun _ -> false) (fun _ -> true) @>
            test <@ "s" |> String.replicate 30 |> create (fun _ -> true) (fun _ -> false) @>

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

    let eqGroup s u = let (N.Name g) = u |> getGroupName in g = s

    module Units =

        module SBCL = StringBCL
        module N = Name

        let name = N.create id (fun m -> m |> N.raiseExc)

        let create' g n a m =
            let g' = name g
            let n' = name n
            let a' = name a
            create id (fun m -> m |> raiseExc) g' n' a' m

        [<Literal>]
        let generalGroup = "General"

        [<Literal>]
        let countGroup = "Count"
        let createCount = create' countGroup
        let count =  createCount "Times" "X" MP.one  
        let countUnits = [count] 

        [<Literal>]
        let massGroup = "Mass"
        let createMass = create' massGroup
        let kiloGram  = createMass "KiloGram"  "kg"    MP.kilo
        let gram      = createMass "Gram"      "g"     MP.one
        let milliGram = createMass "MilliGram" "mg"    MP.milli
        let microGram = createMass "MicroGram" "mcg"   MP.micro
        let nanoGram  = createMass "NanoGram"  "nanog" MP.nano
        let massUnits = [kiloGram;gram;milliGram;microGram;nanoGram]

        [<Literal>]
        let molarGroup = "Molar"
        let createMolar = create' molarGroup
        let mol      = createMass "Mol"      "mol"  MP.one  
        let milliMol = createMass "MilliMol" "mmol" MP.milli  
        let molarUnits = [mol;milliMol] 

        [<Literal>]
        let weightGroup = "Weight"
        let createWeight = create' weightGroup
        let weightKg   = createWeight "KiloGram" "kg" MP.kilo 
        let weightGram = createWeight "Gram"     "g"  MP.one
        let weightUnits = [weightKg;weightGram] 

        [<Literal>]
        let bsaGroup = "BSA"
        let createBSA = create' bsaGroup
        let bsa = createBSA "SquareMeter" "m^s" MP.kilo 
        let bsaUnits = [bsa]

        [<Literal>]
        let volumeGroup = "volumeUnits"
        let createVolume = create' volumeGroup
        let liter      = createVolume "Liter"      "l"   MP.one
        let deciLiter  = createVolume "DeciLiter"  "dl"  MP.deci 
        let milliLiter = createVolume "MilliLiter" "ml"  MP.milli 
        let microLiter = createVolume "MicroLiter" "mcl" MP.micro 
        let volumeUnits = [liter;deciLiter;milliLiter;microLiter]

        [<Literal>]
        let timeGroup = "Time"
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
        let distanceGroup = "Distance"
        let createDistance = create' distanceGroup
        let meter      = createDistance "Meter"      "m"  MP.one 
        let centimeter = createDistance "CentiMeter" "cm" MP.centi 
        let distanceUnits = [meter;centimeter]

        let units = [countUnits;massUnits;molarUnits;weightUnits;bsaUnits;volumeUnits;timeUnits;distanceUnits]

        let hasName s u = 
            let eqs = N.eqs s
            let (n, ns) = getName u
            n |> eqs || ns |> List.exists eqs

        let find succ fail us s = 
            let u =
                us 
                |> List.collect id
                |> List.tryFind (hasName s)
            match u with 
            | Some u -> u |> succ
            | None   -> s |> UnitNotFound |> fail

        let find' us succ fail = find succ fail us
        let weightFromString succ fail   = find' [weightUnits] succ fail
        let distanceFromString succ fail = find' [distanceUnits] succ fail
        let adjustFromString succ fail   = find' [weightUnits;bsaUnits] succ fail
        let timeFromString succ fail     = find' [timeUnits] succ fail

        let fromString s =
            match s |> find Some (fun _ -> None) units with
            | Some(u) -> u |> Some
            | None -> 
                let name = N.create Some (fun _ -> None)
                let g = name generalGroup
                let n = name s
                let a = name s 
                match g, n, a with
                | Some (g'), Some (n'), Some (a') -> 
                    create Some (fun _ -> None) g' n' a' MP.one
                | _ -> None

        let isGroup gr u = u |> getGroupName |> N.eqs gr
        let isTime = isGroup timeGroup
        let isVolume = isGroup volumeGroup
        let isAdjust u = u |> isGroup weightGroup || (u |> isGroup bsaGroup)
         

module TestUnit =

    open Swensen.Unquote
    module UN = Unit

//    let ``Creating a unit with an empty name will fail`` =
//        fun () ->
//            
//            test <@ UN.create (fun _ -> false) (fun _ -> true) "" "" "" 1N  @>
//
//    let ``Creating a unit mass kilogram multiplier 1 abbreviation kg will succeed`` =
//        fun () ->
//            test <@ UN.create (fun _ -> true) (fun _ -> false) "mass" "kilogram" "kg" 100N @>

    let ``Converting 2 kilo to basse gram equals 2000`` =
        fun () ->
            test<@ 2N |> UN.toBase UN.Units.kiloGram = 2000N @>

    let ``Converting 2000 gram to to unit kilo equals 2`` =
        fun () ->
            test<@ 2000N |> UN.toUnit UN.Units.kiloGram = 2N @>

    let ``Converting 1 milliGram to microGram equals 1000`` =
        fun () ->
            let unitToUnit = UN.unitToUnit id (fun m -> m |> UN.raiseExc)
            test<@ 1N |> unitToUnit UN.Units.milliGram UN.Units.microGram = 1000N @>

    let run() =
        [
            ``Converting 2 kilo to basse gram equals 2000``
            ``Converting 2000 gram to to unit kilo equals 2``
            ``Converting 1 milliGram to microGram equals 1000``
        ] |> List.iter (fun t -> t())

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ValueUnit =

    module MP = Multipliers
    module UN = Unit

    type ValueUnit = 
    | ValueUnit of BigRational * UN.Unit * (Operator * BigRational * UN.Unit) list
    and Operator =
    | Per
    | Times
        
    let create v u = (v , u, []) |> ValueUnit

    let get (ValueUnit(v, u, ul)) = v, u, ul

    let operator op v u vu =
        let v', u', ul = vu |> get
        (v', u', ul @ [(op, v, u)]) 
        |> ValueUnit

    let withUnit u v = create v u

    let per = operator Per
    
    let times = operator Times

    let getMultiplier vu = 
        let v, u, ul = vu |> get
        let mp v u = v * (u |> UN.getMultiplier)

        ul 
        |> List.fold (fun acc (op, v, u) ->
            match op with
            | Per   -> acc / (mp v u)
            | Times -> acc * (mp v u)) (mp v u)
            
    let toBase vu v = v |> MP.toBase (vu |> getMultiplier)

    let toUnit vu v = v |> MP.toUnit (vu |> getMultiplier)

    let eval x =
        let _, u, ul = x |> get

        let sort xs =
            xs |> List.sortWith(fun x1 x2 -> 
                let op1, v1, _ = x1
                let op2, v2, _ = x2 
                match op1, op2 with
                | Times, Times -> if v1 > v2 then -1 else 0
                | Times, Per   -> -1
                | Per,  Times  -> +1
                | Per,  Per    -> 0)

        let eqs x1 x2 =
            let op1, v1, u1 = x1
            let op2, v2, u2 = x2
            let opeq = op1 = op2
            let greq = u1 |> UN.getGroupName = (u2 |> UN.getGroupName)
            (opeq |> not) && greq 

        let rec simplify acc list = 
            let remCount xs = 
                xs 
                |> List.filter(fun x -> 
                    let (_, _, u) = x
                    u |> UN.eqGroup UN.Units.countGroup |> not) 
                
            let rec remove i l =
                match i, l with
                | 0, x::xs -> xs
                | i, x::xs -> x::remove (i - 1) xs
                | i, [] -> failwith "index out of range"

            match list with
            | [] -> 
                let acc = acc |> remCount |> sort
                match acc with
                | [(Per, _, _)] -> (Times, 1N, UN.Units.count)::acc
                | _             -> acc
            | x::xs -> 
                match xs |> List.tryFindIndex (eqs x) with
                | Some i -> 
                    xs |> remove i |> simplify acc
                | None -> xs |> simplify (acc @ [x])
                    
        match simplify [] ((Times, 1N, u)::ul) with
        | [] -> create 1N UN.Units.count
        | x::xs -> 
            let _, _, u = x
            (1N, u, xs) |> ValueUnit

    let (|Mult|Div|) op =
        match op with
        | _ when 1N |> op <| 2N = 2N      -> Mult
        | _ when 1N |> op <| 2N = (1N/2N) -> Div
        | _ -> failwith "Not a valid operator"

    let calc op vu1 vu2 = 
        let op' = 
            match op with
            | Mult -> Times
            | Div  -> Per
        let v1, u1, ul1 = vu1 |> get
        let v2, u2, ul2 = vu2 |> get
        (v1, u1, ul1 @ [op', v2, u2] @ ul2) 
        |> ValueUnit
        |> eval
        

open ValueUnit
open Unit.Units

let vu = 1N |> withUnit milliGram |> per 2N day
vu |> getMultiplier
10N |> toBase vu 
(1N/8640000N) |> toUnit vu


