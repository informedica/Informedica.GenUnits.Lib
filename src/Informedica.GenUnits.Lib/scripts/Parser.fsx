
#I __SOURCE_DIRECTORY__

#r "nuget: MathNet.Numerics"
#r "nuget: Informedica.Utils.Lib"
#r "nuget: FParsec"

#load @"../ValueUnit.fs"
#load @"../Parser.fs"
#load @"../Api.fs"

open Informedica.GenUnits.Lib

"1 x / 3 weken"
|> Parser.parse

"10 mg / kg(w) / dag"
|> Parser.parse
|> function
| Some x ->
    "10 kg(w)"
    |> Parser.parse
    |> function
    | Some w -> w * x
    | None -> failwith "cannot parse"
| None ->
    failwith "cannot parse"
