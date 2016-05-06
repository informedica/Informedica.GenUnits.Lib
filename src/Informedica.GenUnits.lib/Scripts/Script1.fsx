#load "load-project-release.fsx"

#time

open Microsoft.FSharp.Reflection

open Informedica.GenUnits.Lib

module TestMultipliers =
    open Swensen.Unquote
    module MP = Unit.Multipliers

    let ``Converting value to base and back to unit equals the value`` =
        fun () -> 
            let c = MP.toBase MP.kilo >> MP.toUnit MP.kilo 
            test<@ 10N |> c = 10N @>

    let run() =
        [
            ``Converting value to base and back to unit equals the value``
        ] |> List.iter (fun t -> t())


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


open CombiUnit
open Unit.Units
open ValueUnit

module CU = CombiUnit
module VU = ValueUnit

let toString = CU.toString
   
let toString2 = VU.toString

let cu1 = 1N |> withUnit milliGram |> per 1N day
cu1 |> toString
let cu2 = 1N |> withUnit day
let cu3 = 1N |> withUnit weightKg
let cu5 = (cu1 / cu3) * cu2

let cu4 = 1N |> withUnit weightGram 

let vu1 = create 100N cu5
let vu2 = create 3500N cu4

vu1 * vu2 |> toString2

let cu6 = 1N |> withUnit count |> per 2N day
let cu7 = 1N |> withUnit milliGram

let vu3 = create 3N cu6
let vu4 = create 20N cu7

(vu3 * vu4) * (create 1N cu2) |> toString2

