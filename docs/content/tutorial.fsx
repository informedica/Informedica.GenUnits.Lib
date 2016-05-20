(*** hide ***)
#I "../../src/Informedica.GenUnits.Lib/Scripts"
#load "load-project-release.fsx"

open Informedica.GenUtils.Lib.BCL

module CS = Informedica.GenUnits.Lib.Constants

let print s = 
    printfn "> %s </br>" s

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

(*** hide **)
``500 mg`` |> VU.toString |> print
``2 dd``   |> VU.toString |> print

(** 
Output:

> 500 mg[Mass] </br>
> 2 X[Count]/day[Time]

*)

(** 
Perform a calculation: 2 X/day \* 500 mg = 1000 mg/day
*)

let tot = ``2 dd`` * ``500 mg`` 
tot |> VU.toString

(*** hide **)
tot |> VU.toString |> print

(** 
Output:

 > 1000 mg[Mass]/day[Time] </br>

*)

(** 
Convert 1000 mg/day to g/week
*)

let ``gram/week`` = 1N |> CU.withUnit gram |> CU.per 1N week
tot |> VU.convertTo ``gram/week`` |> VU.toString

(*** hide **)
tot |> VU.convertTo ``gram/week`` |> VU.toString |> print

(** 
Output:

> 7 g[Mass]/week[Time]

*)

(** 
## Create a combi unit directly from a string

*)

"mg[Mass]/kg[Weight]/2 day[Time]" |> CU.fromString

(** 

    Combi
    (1N,{Group = Name "Mass"; 
            Name = (Name "MilliGram", []); 
            Abbreviation = (Name "mg", []); 
            Multiplier = 1/1000N;}, 
        [(Per, 1N, {Group = Name "Kg"; 
                    Name = (Name "kg", []); 
                    Abbreviation = (Name "kg", []); 
                    Multiplier = 1N;}); 
        (Per, 2N, {Group = Name "Time"; 
                    Name = (Name "Day", []); 
                    Abbreviation = (Name "day", []); 
                    Multiplier = 86400N;})])

*)

(** 

And a value with a unit

*)

"20 mg[Mass]/kg[Weight]/2 day[Time]" |> VU.fromString
tot |> VU.convertTo ``gram/week`` |> VU.toString |> VU.fromString
``2 dd`` |> VU.toString |> VU.fromString

(** 

    ValueUnit 
    (2N, 
        Combi (1N,{Group = Name "Count"; 
                Name = (Name "Times", []);  
                Abbreviation = (Name "X", []); 
                Multiplier = 1N;},[(Per, 1N, {Group = Name "Time"; 
                                                Name = (Name "Day", []); 
                                                Abbreviation = (Name "day", []); 
                                                Multiplier = 86400N;})])) 

*)


(** 
## Evaluate an expression

*)

"2 mg[Mass] * 3 X[Count]/day[Time]" |> Api.eval

(** 

> "6 mg[Mass]/day[Time]"

*)

let conc = "200 mg[Mass] / 50 ml[Volume]" |> Api.eval
let rate = "2 mL[Mass]/hour[Time]"      |> Api.eval
let dose = 
    rate + " * " + conc + " / 60 kg[Weight]" 
    |> Api.eval
    |> Api.convert "mcg[Mass]/kg[Weight]/min[Time]"


(** 

> val conc : string = "4 mg[Mass]/ml[Volume]" </br>
> val rate : string = "2 ml[Ml]/hr[Time]" </br>
> val dose : string = "20000/9 mcg[Mass]/kg[Kg]/min[Time]" 

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

> kg[Mass]/kg[Weight]/sec[Time] </br>
> kg[Mass]/kg[Weight]/min[Time] </br>
> kg[Mass]/kg[Weight]/hr[Time] </br>
> </br>
> .... </br>
> </br>
> nanog[Mass]/g[Weight]/week[Time] </br>
> nanog[Mass]/g[Weight]/mo[Time] </br>
> nanog[Mass]/g[Weight]/yr[Time]

*)


(** 
When a unit is 'unknown', i.e. not defined in the library, a special unit group is created.

*)

"tablet(Shape)" 
|> CU.fromString

(** 

     CombiUnit.CombiUnit = Combi (1N,{Group = Name "Shape"; 
                                      Name = (Name "tablet", []); 
                                      Abbreviation = (Name "tablet", []); 
                                      Multiplier = 1N;},[]) 

*)