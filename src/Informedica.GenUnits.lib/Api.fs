namespace Informedica.GenUnits.Lib

module Api =

    module C = Constants
    module SBCL = Informedica.GenUtils.Lib.BCL.String
    module CU = CombiUnit
    module VU = ValueUnit
    
    let fromString = VU.fromString

    let toString = VU.toString

    let eval s = 
        let addSpace s = C.space + s + C.space
        let mults  = C.mults |> addSpace
        let divs   = C.divs  |> addSpace
        let adds   = "+"     |> addSpace
        let subtrs = "-"     |> addSpace 

        let del = "#"
        let addDel s = del + s + del

        let opts s = 
            match s with
            | _ when s = C.mults -> (*)
            | _ when s = C.divs  -> (/)
            | _ when s = "+"     -> (+)
            | _ when s = "-"     -> (-)
            | _ -> failwith "Cannot evaluate string"

        let rec eval' acc terms =
            if acc |> Option.isNone then 
                eval' (terms |> List.head |> VU.fromString |> Some) (terms |> List.tail)
            else
                match terms with
                | [] -> acc |> Option.get
                | os::vus::rest ->
                    let op = os |> opts
                    let vu = vus |> VU.fromString
                    rest |> eval' ((acc |> Option.get) |> op <| vu |> Some) 
                | _ -> failwith "Cannot evaluate string"          

        s 
        |> SBCL.replace mults  (C.mults |> addDel)
        |> SBCL.replace divs   (C.divs  |> addDel)
        |> SBCL.replace adds   (adds    |> addDel)
        |> SBCL.replace subtrs (subtrs  |> addDel)
        |> SBCL.split del
        |> eval' None
        |> VU.toString

    let convert s2 s1 = 
        let vu = s1 |> VU.fromString
        let cu = s2 |> CU.fromString
        vu 
        |> VU.convertTo cu
        |> toString