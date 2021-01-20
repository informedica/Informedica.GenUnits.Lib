
#I __SOURCE_DIRECTORY__

#r "nuget: MathNet.Numerics"
#r "nuget: Informedica.Utils.Lib"
#r "nuget: FParsec"

#load @"../ValueUnit.fs"
#load @"../Parser.fs"
#load @"../Api.fs"

open Informedica.GenUnits.Lib
open ValueUnit

// Example parsing a frequency
"1 x / 3 weken"
|> Parser.parse

// Example parsing a dose
// using kg as a weight
let parse =
    let exclude u (ud : Units.UnitDetails) = ud.Unit <> u
    ValueUnit.Units.units
    // leave out kg as mass so only kg as weigh is recognized
    |> List.filter (exclude Units.Mass.kiloGram)
    // leave out gr as weight to avoid confusion with gr as mass
    |> List.filter (exclude Units.Weight.gram)
    |> Parser.parseWitUnits

"10 mg / kg / dag"
|> parse
|> function
| Some x ->
    "10 kg"
    |> parse
    |> function
    | Some w -> w * x
    | None -> failwith "cannot parse"
| None ->
    failwith "cannot parse"

// failing example of combi unit
"10 gram/10 gram"
|> parse
