namespace Informedica.GenUnits.Lib

open Informedica.GenUtils.Lib.BCL

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]        
module ValueUnit =

    module SBCL = Informedica.GenUtils.Lib.BCL.String
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

    let canConvert cu vu =
        let v, cu1 = vu |> get
        let _, u1, ul1 = cu1 |> CU.get
        let _, u2, ul2 = cu  |> CU.get

        let eq u1 u2 = u1 |> UN.getGroupName = (u2 |> UN.getGroupName)

        let canConvUl ul1 ul2 =
            ul1 |> List.forall2 (fun (o1, _, u1) (o2, _, u2) ->
                o1 = o2 && u1 |> eq u2
            ) ul2

        u1 |> eq u2 && canConvUl ul1 ul2     

    let convertTo cu vu =
        let v, cu1 = vu |> get
        let _, u1, ul1 = cu1 |> CU.get
        let _, u2, ul2 = cu  |> CU.get

        let v' = v |> CU.toBase cu1 |> CU.toUnit cu
        (v', cu) |> ValueUnit

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
