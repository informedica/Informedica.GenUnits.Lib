(*** hide ***)
#I "../../src/Informedica.GenUnits.Lib/Scripts"
#load "load-project-release.fsx"

#time

(**
Create a `ValueUnit`
*)

open Informedica.GenUnits.Lib
open Informedica.GenUnits.Lib.Unit.Units

module CU = CombiUnit
module VU = ValueUnit
module UG = UnitGroup

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

(** 
## Create a combi unit directly from a string

*)

"mg/kg/2 day" |> CU.fromString

(** 

And a value with a unit

*)

"20 mg/kg/2 day" |> VU.fromString
tot |> VU.convertTo ``gram/week`` |> VU.toString |> VU.fromString
``2 dd`` |> VU.toString |> VU.fromString

(** 
## Evaluate an expression

*)

"2 mg * 3 X/day" |> Api.eval

let conc = "200 mg / 50 ml" |> Api.eval
let rate = "2 mL/hour"      |> Api.eval
let dose = 
    rate + " * " + conc + " / 60 kg_weight" 
    |> Api.eval
    |> Api.convert "mcg/kg_weight/min"

(** 
## Calculating with Unit Groups

It is also possible to perform 'unit group calculations'
*)

let ug1 = "Mass" |> UG.fromString
let ug2 = "Weight" |> UG.fromString
let ug3 = "Time" |> UG.fromString

let cg = ug1 / ug2 / ug3
cg |> UG.toString

(** 
Results in:

> val it : string = "Mass/Weight/Time"

From the combined unit group a list of possible units can be generated
*)

cg |> UG.getUnits
|> List.map CU.toString
|> List.iter (printfn "> %s </br>") 

(** Results in: 

> kg/kg/sec </br>
> kg/kg/min </br>
> kg/kg/hr </br>
> kg/kg/day </br>
> kg/kg/week </br>
> kg/kg/mo </br>
> kg/kg/yr </br>
> kg/g/sec </br>
> kg/g/min </br>
> kg/g/hr </br>
> kg/g/day </br>
> kg/g/week </br>
> kg/g/mo </br>
> kg/g/yr </br>
> g/kg/sec </br>
> g/kg/min </br>
> g/kg/hr </br>
> g/kg/day </br>
> g/kg/week </br>
> g/kg/mo </br>
> g/kg/yr </br>
> g/g/sec </br>
> g/g/min </br>
> g/g/hr </br>
> g/g/day </br>
> g/g/week </br>
> g/g/mo </br>
> g/g/yr </br>
> mg/kg/sec </br>
> mg/kg/min </br>
> mg/kg/hr </br>
> mg/kg/day </br>
> mg/kg/week </br>
> mg/kg/mo </br>
> mg/kg/yr </br>
> mg/g/sec </br>
> mg/g/min </br>
> mg/g/hr </br>
> mg/g/day </br>
> mg/g/week </br>
> mg/g/mo </br>
> mg/g/yr </br>
> mcg/kg/sec </br>
> mcg/kg/min </br>
> mcg/kg/hr </br>
> mcg/kg/day </br>
> mcg/kg/week </br>
> mcg/kg/mo </br>
> mcg/kg/yr </br>
> mcg/g/sec </br>
> mcg/g/min </br>
> mcg/g/hr </br>
> mcg/g/day </br>
> mcg/g/week </br>
> mcg/g/mo </br>
> mcg/g/yr </br>
> nanog/kg/sec </br>
> nanog/kg/min </br>
> nanog/kg/hr </br>
> nanog/kg/day </br>
> nanog/kg/week </br>
> nanog/kg/mo </br>
> nanog/kg/yr </br>
> nanog/g/sec </br>
> nanog/g/min </br>
> nanog/g/hr </br>
> nanog/g/day </br>
> nanog/g/week </br>
> nanog/g/mo </br>
> nanog/g/yr </br>

*)


(** 
When a unit is 'unknown', i.e. not defined in the library, a special unit group is created.

*)

"Tablet" 
|> CU.fromString

(** 

> val it : CombiUnit.CombiUnit = Combi (1N,{Group = Name "Tablet";
>                                           Name = (Name "tablet", []);
>                                           Abbreviation = (Name "tablet", []);
>                                           Multiplier = 1N;},[])

*)