namespace Informedica.GenUnits.Lib

open Informedica.GenUtils.Lib.BCL

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UnitGroup =

    open Informedica.GenUnits.Lib

    module UN = Unit
    module NM = UN.Name
    module CU = CombiUnit

    type UnitGroup = UnitGroup of NM.Name * (CU.Operator * NM.Name) list

    let create n = (n |> NM.Name, []) |> UnitGroup

    let nameToString (NM.Name n) = n

    let opToString = function
        | CU.Per -> "/"
        | CU.Times -> "*"

    let apply f (ug: UnitGroup) = ug |> f

    let get = apply id

    let getAll (UnitGroup(n, nl)) = n, nl 

    let addGroup o n ug = 
        let g, gl = ug |> getAll
        (g, [(o, n |> NM.Name)] |> List.append gl) |> UnitGroup

    let perGroup = addGroup CU.Per

    let timesGroup = addGroup CU.Times

    let fromUnit cu = 
        let _, u, ul = cu |> CU.get
        (u |> UN.getGroupName, ul |> List.map (fun (op, _, u) -> op, u |> UN.getGroupName))
        |> UnitGroup

    let toString ug =
        let n, nl = ug |> getAll
        (n |> nameToString)::(nl |> List.map (fun (op, n) -> (op |> opToString) + (n |> nameToString) ))
        |> String.concat ""

    let fromString s =
        let dels = "#"
        let mults = "*"
        let divs  = "/"
        let space = " "

        let ofs s =
            match s with
            | _ when s = mults -> CU.Times
            | _ when s = divs  -> CU.Per
            | _ -> failwith "Not a valid operator string"

        let rec parse ul usl =
            match usl with
            | [us] -> 
                let u = us |> NM.Name
                (u, ul) |> UnitGroup
            | us::os::rest -> 
                let u = us |> NM.Name
                let o = os |> ofs
                rest |> parse ([ (o, u)] @ ul)
            | _ -> failwith "Cannot parse string list"

        s
        |> String.replace mults (dels + mults + dels)
        |> String.replace divs  (dels + divs + dels)
        |> String.split dels
        |> List.rev
        |> parse []

    let eqs ug u = u |> fromUnit = ug

    let getUnits ug =
        let n, nl = ug |> getAll

        let get n = 
            UN.Units.units
            |> List.find (fun us -> us.Head.Group = n)

        let us, usl = n |> get, nl |> List.map (fun (o, u) -> o, u |> get)
            
        let rec create usl cul =
            match usl with
            | [] -> cul
            | (o, ul)::tail ->
                let f =  match o with | CU.Per -> CU.per 1N | CU.Times -> CU.times 1N
                [
                    for cu in cul do
                        for u in ul do
                            yield cu |> f u
                ] |> create tail
            
        create usl (us |> List.map (fun u -> 1N |> CU.withUnit u))

    let calc op ug1 ug2 =  
        let cu1, cu2 = ug1 |> getUnits |> List.head, ug2 |> getUnits |> List.head
        (cu1 |> op <| cu2)
        |> fromUnit

    type UnitGroup with
        
        static member (*) (cu1, cu2) = calc (*) cu1 cu2

        static member (/) (cu1, cu2) = calc (/) cu1 cu2

        static member (+) (cu1, cu2) = calc (+) cu1 cu2

        static member (-) (cu1, cu2) = calc (-) cu1 cu2

