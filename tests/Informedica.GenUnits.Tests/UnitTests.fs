namespace Informedica.GenUnits.Tests

open Swensen.Unquote
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Informedica.GenUnits.Lib


module UnitTests =
    
        module NameTests =
            
            module N = Informedica.GenUnits.Lib.Unit.Name     
            
            let create = N.create       

            [<TestFixture>]
            type ``Given an empty or null string`` () =                    
                
                [<Test>]
                member x.``Name cannot be created`` () =
                    let succ _ = false
                    let fail _ = true
                    let create = create succ fail
                    test <@ create ""  @>
                    test <@ create null @>

            [<TestFixture>]
            type ``Given a string`` () =

                [<Property>]
                member x.``Name should be at least one character long`` () =
                    (fun s -> 
                        let succ _ = s |> String.length >= 1
                        let fail _ = true
                        s |> create succ fail )

                [<Property>]
                member x.``Name should be smaller than 30 characters`` () =
                    (fun s -> 
                        let succ _ = s |> String.length <= 30
                        let fail _ = true
                        s |> create succ fail )

        module UnitsTests =
            
            open Informedica.GenUnits.Lib

            module UG = UnitGroup
            module UN = Unit
            module US = UN.Units
            module NM = Unit.Name

            let unitGroups =
                US.units 
                |> List.collect (List.map (fun u -> 
                    (u |> UN.getName |> fst |> NM.toString, u |> UN.getGroupName |> NM.toString)))


            [<TestFixture>]
            type ``For every unit and group name combination`` () =
                
                [<Test>]
                member x.``Unit and group can be generate from string and returned from string`` () =
                    for (u, g) in unitGroups do
                        test<@ US.fromString u g |> Option.isSome @>

            [<TestFixture>]
            type ``For every group of units`` () =
                let groups = 
                    US.units
                    |> List.map (fun us -> us.Head.Group)

                [<Test>]
                member x.``The number of units equals the units in the group`` () =
                    let c1 = 
                        groups
                        |> List.collect (fun n -> (n, []) |> UG.UnitGroup |> UG.getUnits)
                        |> List.length
                    let c2 = US.units |> List.collect id |> List.length
                    test <@ c1 = c2 @>

