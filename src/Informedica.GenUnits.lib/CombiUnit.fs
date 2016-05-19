namespace Informedica.GenUnits.Lib

open Informedica.GenUtils.Lib.BCL

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CombiUnit =

    module C = Constants

    [<Literal>] 
    let mults = C.mults
    [<Literal>] 
    let divs  = C.divs
    [<Literal>]
    let empts = C.empts
    [<Literal>]
    let space = C.space

    module SBCL = Informedica.GenUtils.Lib.BCL.String
    module UN = Unit
    module MP = UN.Multipliers
    module NM = UN.Name

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
            | _ -> failwith "Not a valid unit operator"
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
        let gr u = u |> Unit.getGroupName |> NM.toString
        let toStr u = (u |> abbr) + "(" + (u |> gr) + ")"

        let bigRatToString (v: BigRational) =
            if v = 1N then empts else v.ToString()

        let v, u, ul = cu |> get
        let acc = (v |> bigRatToString) + space + (u |> toStr) |> String.trim
        ul 
        |> List.fold (fun acc (o, v, u) -> 
                let v' = v |> bigRatToString
                let o' = match o with | Times -> mults | Per -> divs
                let u' = u |> toStr
                acc +
                if v' = empts then o' + u' else v' + space + o' + u') acc

    let fromString s =
        let dels = "#"
        let getUnitAndGroup ug = 
            match ug |> String.replace ")" "" |> String.split "(" with
            | [u;g] -> u, g
            | _ -> sprintf "Could not parse unit from string: %s" ug |> failwith

        let ofs s =
            match s with
            | _ when s = mults -> Times
            | _ when s = divs  -> Per
            | _ -> failwith "Not a valid operator string"

        let ufs s =
            match s |> SBCL.split space with
            | [ug] ->
                let u, g = ug |> getUnitAndGroup 
                match u |> UN.Units.fromString g with
                | Some (u) -> 1N, u
                | None     -> failwith "Not a valid unit"
            | [v;ug] -> 
                let u, g = ug |> getUnitAndGroup 
                let v' = v |> BigRational.Parse
                match u |> UN.Units.fromString g with
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


