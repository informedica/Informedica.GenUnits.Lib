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

> val it : string = "500 mg(Mass)"
> val it : string = "2 X(Count)/day(Time)"

*)

(** 
Perform a calculation: 2 X/day * 500 mg = 1000 mg/day
*)

let tot = ``2 dd`` * ``500 mg`` 
tot |> VU.toString

(** 
Output:

 > val it : string = "1000 mg(Mass)/day(Time)"

*)

(** 
Convert 1000 mg/day to g/week
*)

let ``gram/week`` = 1N |> CU.withUnit gram |> CU.per 1N week
tot |> VU.convertTo ``gram/week`` |> VU.toString

(** 
Output:

> val it : string = "7 g(Mass)/week(Time)"

*)

(** 
## Create a combi unit directly from a string

*)

"mg(Mass)/kg(Weight)/2 day(Time)" |> CU.fromString

(** 

And a value with a unit

*)

"20 mg(Mass)/kg(Weight)/2 day(Time)" |> VU.fromString
tot |> VU.convertTo ``gram/week`` |> VU.toString |> VU.fromString
``2 dd`` |> VU.toString |> VU.fromString

(** 
## Evaluate an expression

*)

"2 mg(Mass) * 3 X(Count)/day(Time)" |> Api.eval

let conc = "200 mg(Mass) / 50 ml(Volume)" |> Api.eval
let rate = "2 mL(Mass)/hour(Time)"      |> Api.eval
let dose = 
    rate + " * " + conc + " / 60 kg(Weight)" 
    |> Api.eval
    |> Api.convert "mcg(Mass)/kg(Weight)/min(Time)"


(** 

> val it : string = "6 mass(Mass)/time(Time)" </br>
> val conc : string = "4 mass(Mass)/volume(Volume)" </br>
> val rate : string = "2 mass(Mass)/time(Time)" </br>
> val dose : string = "2/15 mass(Mass)/weight(Weight)/time(Time)" </br>

*)

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

> 
> kg(Mass)/kg(Weight)/sec(Time) </br>
> kg(Mass)/kg(Weight)/min(Time) </br>
> kg(Mass)/kg(Weight)/hr(Time) </br>
> kg(Mass)/kg(Weight)/day(Time) </br>
> kg(Mass)/kg(Weight)/week(Time) </br>
> kg(Mass)/kg(Weight)/mo(Time) </br>
> kg(Mass)/kg(Weight)/yr(Time) </br>
> kg(Mass)/g(Weight)/sec(Time) </br>
> kg(Mass)/g(Weight)/min(Time) </br>
> kg(Mass)/g(Weight)/hr(Time) </br>
> kg(Mass)/g(Weight)/day(Time) </br>
> kg(Mass)/g(Weight)/week(Time) </br>
> kg(Mass)/g(Weight)/mo(Time) </br>
> kg(Mass)/g(Weight)/yr(Time) </br>
> g(Mass)/kg(Weight)/sec(Time) </br>
> g(Mass)/kg(Weight)/min(Time) </br>
> g(Mass)/kg(Weight)/hr(Time) </br>
> g(Mass)/kg(Weight)/day(Time) </br>
> g(Mass)/kg(Weight)/week(Time) </br>
> g(Mass)/kg(Weight)/mo(Time) </br>
> g(Mass)/kg(Weight)/yr(Time) </br>
> g(Mass)/g(Weight)/sec(Time) </br>
> g(Mass)/g(Weight)/min(Time) </br>
> g(Mass)/g(Weight)/hr(Time) </br>
> g(Mass)/g(Weight)/day(Time) </br>
> g(Mass)/g(Weight)/week(Time) </br>
> g(Mass)/g(Weight)/mo(Time) </br>
> g(Mass)/g(Weight)/yr(Time) </br>
> mg(Mass)/kg(Weight)/sec(Time) </br>
> mg(Mass)/kg(Weight)/min(Time) </br>
> mg(Mass)/kg(Weight)/hr(Time) </br>
> mg(Mass)/kg(Weight)/day(Time) </br>
> mg(Mass)/kg(Weight)/week(Time) </br>
> mg(Mass)/kg(Weight)/mo(Time) </br>
> mg(Mass)/kg(Weight)/yr(Time) </br>
> mg(Mass)/g(Weight)/sec(Time) </br>
> mg(Mass)/g(Weight)/min(Time) </br>
> mg(Mass)/g(Weight)/hr(Time) </br>
> mg(Mass)/g(Weight)/day(Time) </br>
> mg(Mass)/g(Weight)/week(Time) </br>
> mg(Mass)/g(Weight)/mo(Time) </br>
> mg(Mass)/g(Weight)/yr(Time) </br>
> mcg(Mass)/kg(Weight)/sec(Time) </br>
> mcg(Mass)/kg(Weight)/min(Time) </br>
> mcg(Mass)/kg(Weight)/hr(Time) </br>
> mcg(Mass)/kg(Weight)/day(Time) </br>
> mcg(Mass)/kg(Weight)/week(Time) </br>
> mcg(Mass)/kg(Weight)/mo(Time) </br>
> mcg(Mass)/kg(Weight)/yr(Time) </br>
> mcg(Mass)/g(Weight)/sec(Time) </br>
> mcg(Mass)/g(Weight)/min(Time) </br>
> mcg(Mass)/g(Weight)/hr(Time) </br>
> mcg(Mass)/g(Weight)/day(Time) </br>
> mcg(Mass)/g(Weight)/week(Time) </br>
> mcg(Mass)/g(Weight)/mo(Time) </br>
> mcg(Mass)/g(Weight)/yr(Time) </br>
> nanog(Mass)/kg(Weight)/sec(Time) </br>
> nanog(Mass)/kg(Weight)/min(Time) </br>
> nanog(Mass)/kg(Weight)/hr(Time) </br>
> nanog(Mass)/kg(Weight)/day(Time) </br>
> nanog(Mass)/kg(Weight)/week(Time) </br>
> nanog(Mass)/kg(Weight)/mo(Time) </br>
> nanog(Mass)/kg(Weight)/yr(Time) </br>
> nanog(Mass)/g(Weight)/sec(Time) </br>
> nanog(Mass)/g(Weight)/min(Time) </br>
> nanog(Mass)/g(Weight)/hr(Time) </br>
> nanog(Mass)/g(Weight)/day(Time) </br>
> nanog(Mass)/g(Weight)/week(Time) </br>
> nanog(Mass)/g(Weight)/mo(Time) </br>
> nanog(Mass)/g(Weight)/yr(Time) </br>

*)


(** 
When a unit is 'unknown', i.e. not defined in the library, a special unit group is created.

*)

"tablet(Shape)" 
|> CU.fromString

(** 

> val it : CombiUnit.CombiUnit = Combi (1N,{Group = Name "Shape";
>                                           Name = (Name "tablet", []);
>                                           Abbreviation = (Name "tablet", []);
>                                           Multiplier = 1N;},[])

*)