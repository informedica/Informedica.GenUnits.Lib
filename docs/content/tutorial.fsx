(*** hide ***)
#I "../../src/Informedica.GenUnits.Lib/Scripts"
#load "load-project-release.fsx"

(**
Create a `ValueUnit`
*)

open Informedica.GenUnits.Lib
open Informedica.GenUnits.Lib.Unit.Units

module CU = CombiUnit
module VU = ValueUnit

let ``500 mg`` = VU.create 500N (1N |> CU.withUnit milliGram)
let ``2 dd``   = VU.create 2N  (1N  |> CU.withUnit count |> CU.per 1N day)

(** 
Print out the *value units*
*)

``500 mg`` |> VU.toString
``2 dd``   |> VU.toString

(** 
Output:

> val it : string = "500 mg"
> val it : string = "2 X/day"

*)

(** 
Perform a calculation: 2 X/day * 500 mg = 1000 mg/day
*)

let tot = ``2 dd`` * ``500 mg`` 
tot |> VU.toString

(** 
Output:

 > val it : string = "1000 mg/day"

*)

(** 
Convert 1000 mg/day to g/week
*)

let ``gram/week`` = 1N |> CU.withUnit gram |> CU.per 1N week
tot |> VU.convertTo ``gram/week`` |> VU.toString

(** 
Output:

> val it : string = "7 g/week"

*)