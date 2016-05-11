namespace Informedica.GenUnits.Lib


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Unit =  

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

    let eqGroup u1 u2 = let g = u1|> getGroupName in u2 |> getGroupName = g

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
            let (a, aa) = getAbbreviation u
            n |> eqs || ns |> List.exists eqs ||
            a |> eqs || aa |> List.exists eqs

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
         
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CombiUnit =

    [<Literal>] 
    let mults = "*"
    [<Literal>] 
    let divs  = "/"
    [<Literal>]
    let empts = ""
    [<Literal>]
    let space = " "

    module SBCL = StringBCL
    module UN = Unit
    module MP = UN.Multipliers

    type CombiUnit = 
    | Combi of BigRational * UN.Unit * (Operator * BigRational * UN.Unit) list
    and Operator =
    | Per
    | Times
        
    let create v u = (v , u, []) |> Combi

    let get (Combi(v, u, ul)) = v, u, ul

    let operator op v u vu =
        let v', u', ul = vu |> get
        (v', u', ul @ [(op, v, u)]) 
        |> Combi

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
            
    let toBase u v = v |> MP.toBase (u |> getMultiplier)

    let toUnit u v = v |> MP.toUnit (u |> getMultiplier)

    let eqGroup cu1 cu2 =
        let _, u1, ul1 = cu1 |> get
        let _, u2, ul2 = cu2 |> get
        u1 |> UN.eqGroup u2 && 
        ul1 
        |> List.forall2 (fun (_, _, u1) (_, _, u2) -> u1 |> UN.eqGroup u2) ul2

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
            let greq = u1 |> UN.eqGroup u2
            (opeq |> not) && greq 

        let rec simplify acc list = 
            let remCount xs = 
                xs 
                |> List.filter(fun x -> 
                    let (_, _, u) = x
                    u |> UN.getGroupName |> UN.Name.get = UN.Units.countGroup |> not) 
                
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
            (1N, u, xs) |> Combi

    let (|Mult|Div|Add|Subtr|) op =
        match op with
        | _ when 1N |> op <| 2N = 2N      -> Mult
        | _ when 1N |> op <| 2N = (1N/2N) -> Div
        | _ when 1N |> op <| 2N = 3N      -> Add
        | _ when 1N |> op <| 2N = -1N     -> Subtr
        | _ -> failwith "Not a valid operator"

    let calc op cu1 cu2 = 
        let op' = 
            match op with
            | Mult -> Times
            | Div  -> Per
        let _, u1, ul1 = cu1 |> get
        let _, u2, ul2 = cu2 |> get
        match op with
        | Mult | Div ->
            (1N, u1, ul1 @ [op', 1N, u2] @ ul2) 
            |> Combi
            |> eval
        | Add | Subtr -> 
            if cu1 |> eqGroup cu2 then cu2
            else failwith "Cannot add units with different unit groups"

    let toString cu =
        let abbr = Unit.getAbbreviation >> fst >> Unit.Name.get

        let bigRatToString (v: BigRational) =
            if v = 1N then empts else v.ToString()

        let v, u, ul = cu |> get
        let acc = (v |> bigRatToString) + space + (u |> abbr) |> SBCL.trim
        ul 
        |> List.fold (fun acc (o, v, u) -> 
                let v' = v |> bigRatToString
                let o' = match o with | Times -> mults | Per -> divs
                let u' = u |> abbr
                acc +
                if v' = empts then o' + u' else v' + space + o' + u') acc

    let fromString s =
        let dels = "#"

        let ofs s =
            match s with
            | _ when s = mults -> Times
            | _ when s = divs  -> Per
            | _ -> failwith "Not a valid operator string"

        let ufs s =
            match s |> SBCL.split space with
            | [u] -> 
                match u |> UN.Units.fromString with
                | Some (u) -> 1N, u
                | None     -> failwith "Not a valid unit"
            | [v;u] -> 
                let v' = v |> BigRational.Parse
                match u |> UN.Units.fromString with
                | Some (u) -> v', u
                | None     -> failwith "Not a valid unit"
            | _ -> failwith "Cannot parse string"

        let rec parse ul usl =
            match usl with
            | [us] -> 
                let v, u = us |> ufs
                (v, u, ul) |> Combi
            | us::os::rest -> 
                let v, u = us |> ufs
                let o = os |> ofs
                rest |> parse ([ (o, v, u)] @ ul)
            | _ -> failwith "Cannot parse string list"

        s 
        |> SBCL.replace mults (dels + mults + dels) 
        |> SBCL.replace divs  (dels + divs + dels)
        |> SBCL.split dels
        |> List.rev
        |> parse []
        

    type CombiUnit with
        
        static member (*) (cu1, cu2) = calc (*) cu1 cu2

        static member (/) (cu1, cu2) = calc (/) cu1 cu2

        static member (+) (cu1, cu2) = calc (+) cu1 cu2

        static member (-) (cu1, cu2) = calc (-) cu1 cu2


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]        
module ValueUnit =

    module SBCL = StringBCL
    module UN = Unit
    module CU = CombiUnit

    type ValueUnit = ValueUnit of BigRational * CU.CombiUnit

    let create v u = (v, u) |> ValueUnit

    let get (ValueUnit(v, u)) = v, u

    let calc op vu1 vu2 =
        let v1, u1 = vu1 |> get
        let v2, u2 = vu2 |> get
        let u = CU.calc op u1 u2
        let v = v1 |> CU.toBase u1 |> op <| (v2 |> CU.toBase u2) |> CU.toUnit u
        create v u

    let convertTo cu vu =
        let v, cu1 = vu |> get
        let _, u1, ul1 = cu1 |> CU.get
        let _, u2, ul2 = cu  |> CU.get

        let eq u1 u2 = u1 |> UN.getGroupName = (u2 |> UN.getGroupName)

        let canConvert ul1 ul2 =
            ul1 |> List.forall2 (fun (o1, _, u1) (o2, _, u2) ->
                o1 = o2 && u1 |> eq u2
            ) ul2

        if u1 |> eq u2 && canConvert ul1 ul2 then
            let v' = v |> CU.toBase cu1 |> CU.toUnit cu
            (v', cu) |> ValueUnit
        else failwith "Cannot convert"

    let toString vu =
        let v, u = vu |> get
        v.ToString() + " " + (u |> CU.toString)

    let fromString s =
        match s |> SBCL.split CU.space with
        | vs::rest ->
            let v = vs |> BigRational.Parse
            let cu = 
                rest
                |> String.concat CU.empts
                |> CU.fromString
            (v, cu) |> ValueUnit
        | _ -> failwith "Cannot parse string"


    type ValueUnit with

        static member (*) (vu1, vu2) = calc (*) vu1 vu2

        static member (/) (vu1, vu2) = calc (/) vu1 vu2

        static member (+) (vu1, vu2) = calc (+) vu1 vu2

        static member (-) (vu1, vu2) = calc (-) vu1 vu2