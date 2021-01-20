#I __SOURCE_DIRECTORY__

#load @"./../../../.paket/load/netstandard2.1/main.group.fsx"

#load @"./../../../src/Informedica.GenUnits.Lib/ValueUnit.fs"
#load @"./../../../src/Informedica.GenUnits.Lib/Api.fs"

#time


module Tests =

    open MathNet.Numerics
    open Informedica.GenUnits.Lib

    open ValueUnit

    let toString = toString Units.English Units.Short

    // Print intermediate results
    let (>>*) u f =
        u |> toString |> printfn "%s"
        f u

    (* CREATE VALUE UNITS *)

    // Some basic units
    let mg400 = 400N |> create Units.Mass.milliGram
    let ml50  = 50N |> create Units.Volume.milliLiter
    let ml5  = 5N |> create Units.Volume.milliLiter
    let l5 = 5N |> create Units.Volume.liter 

    // The count group is a special unit group 
    // with only one unit: times.
    let times3 = 3N |> create Units.Count.times
    // Division or mutliplication with times
    // results in unchanged units:
    // 3 times * 5 ml = 15 ml
    times3 * ml5

    (* GET BASE AND UNIT VALUES *)

    // Get the base value of the units
    ml5 |> toBase       // 1/200N i.e. 5 ml = 1/200 = 0.005 l
    l5  |> toBase      // 5N i.e. 5 l = 5 l

    // Get the unit values
    ml5 |> toUnit     // 500N i.e. 5 l = 5000 ml
    l5  |> toUnit    // 5 i.e. 5 l = 5 l

    (* USE OF COMPARISON OPERATORS *)

    // Normal comparison operators don't work
    (ml50 > l5) // Returns true, but is false

    // Use the specific comparison operators
    (ml50 >? l5) // Returns correct false


    (* CALCULATION WITH VALUE UNITS *)

    // All four basic arrythmic operations can be performed
    
    // - multiplication
    times3 * times3 // = 9 times
    // - division
    times3 / times3 // = 1 times
    // - addition
    times3 + times3 // = 6 times
    // - subtraction 
    times3 - times3 // = 0 times

    // Addition and subtraction can only be performed within same unitgroups
    (ml50 + l5)    // = 5.05 l
    (mg400 + ml50) // System.Exception: cannot add or subtract different units Mass (MilliGram 1N) Volume (MilliLiter 1N)
    
    // When two valueunits with the same unitgroup are divided you get a count group
    let (_, u) = (l5 / ml50) |> get  // = 100N times
    u |> Group.unitToGroup           // now is a count group

    // Calculation with units
    ((mg400 + mg400)/ ml50)     // (400 mg[Mass] + 400 mg[Mass]) / 50 ml[Volume] = 16 mg[Mass]/ml[Volume]
    >>* ((*) ml50)              // 16 mg[Mass]/ml[Volume] * 50 ml[Volume] = 800 mg[Mass] 
    >>* (fun vu -> vu / ml50)   // 800 mg[Mass] / 50 ml[Volume] = 16 mg[Mass]/ml[Volume]
    >>* ((*) ml50)              // 16 mg[Mass]/ml[Volume] * 50 ml[Volume] = 800 mg[Mass]
    |> toString

    (* VALUE UNITS CAN BE CONVERTED TO DIFFERENT UNITS WITHIN THE SAME UNIT GROUP *)

    // Unit conversion
    l5                            // 5 l[Volume]
    ==> Units.Volume.milliLiter   // = 5000 ml[Volume]
    |> toString

    // Calculate and get the resulting unit group
    4N
    |> create (Units.General.general "dose") // 4 dose[General]
    >>* (fun vu -> vu / (1N |> create Units.Time.day)) // divide by 1 day[Time]
    >>* (fun vu -> vu ==> (Units.General.general "dose" |> per (Units.Time.week)))
    |> (fun (ValueUnit(_, u)) ->
        u |> Group.unitToGroup
    ) // GeneralGroup "dose", OpPer, TimeGroup -> i.e. Dose/Time


    // Calculate and get all valid units for conversion
    mg400 / ml50 / (1N |> create Units.Time.day) // 8 mg[Mass]/ml[Volume]/day[Time]
    >>* (fun vu -> 
        let (_, u) = vu |> get
        u 
        |> Group.unitToGroup
        |> Group.getUnits
        |> List.iter (fun u ->
            create u 1N
            |> toString
            |> printfn "%s"
        )
    )

    // Prints out:
    //1 kg[Mass]/l[Volume]/yr[Time]
    //1 kg[Mass]/l[Volume]/mo[Time]
    //1 kg[Mass]/l[Volume]/wk[Time]
    //1 kg[Mass]/l[Volume]/day[Time]
    //1 kg[Mass]/l[Volume]/hr[Time]
    //1 kg[Mass]/l[Volume]/min[Time]
    //1 kg[Mass]/l[Volume]/sec[Time]
    //1 kg[Mass]/dl[Volume]/yr[Time]
    //1 kg[Mass]/dl[Volume]/mo[Time]
    //1 kg[Mass]/dl[Volume]/wk[Time]
    //1 kg[Mass]/dl[Volume]/day[Time]
    //1 kg[Mass]/dl[Volume]/hr[Time]
    //1 kg[Mass]/dl[Volume]/min[Time]
    //1 kg[Mass]/dl[Volume]/sec[Time]
    //1 kg[Mass]/ml[Volume]/yr[Time]
    //1 kg[Mass]/ml[Volume]/mo[Time]
    //1 kg[Mass]/ml[Volume]/wk[Time]
    //1 kg[Mass]/ml[Volume]/day[Time]
    //1 kg[Mass]/ml[Volume]/hr[Time]
    //1 kg[Mass]/ml[Volume]/min[Time]
    //1 kg[Mass]/ml[Volume]/sec[Time]
    //1 kg[Mass]/microl[Volume]/yr[Time]
    //1 kg[Mass]/microl[Volume]/mo[Time]
    //1 kg[Mass]/microl[Volume]/wk[Time]
    //1 kg[Mass]/microl[Volume]/day[Time]
    //1 kg[Mass]/microl[Volume]/hr[Time]
    //1 kg[Mass]/microl[Volume]/min[Time]
    //1 kg[Mass]/microl[Volume]/sec[Time]
    //1 g[Mass]/l[Volume]/yr[Time]
    //1 g[Mass]/l[Volume]/mo[Time]
    //1 g[Mass]/l[Volume]/wk[Time]
    //1 g[Mass]/l[Volume]/day[Time]
    //1 g[Mass]/l[Volume]/hr[Time]
    //1 g[Mass]/l[Volume]/min[Time]
    //1 g[Mass]/l[Volume]/sec[Time]
    // etc ...