namespace Informedica.GenUnits.Tests

open Swensen.Unquote
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Informedica.GenUnits.Lib


module UnitGroupTests =
    
    [<TestFixture>]
    type ``Given an arbitrary unit group`` () =
        
        let ung = UnitGroup.create "Test"

        [<Test>]
        member x.``The unitgroup only has a unit with the same name`` () =
            test <@ ung |> UnitGroup.getUnits |> List.length = 1 @>
            test <@ ung |> UnitGroup.getUnits |> List.head |> CombiUnit.toString = "test[Test]" @>