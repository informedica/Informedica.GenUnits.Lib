namespace Informedica.GenUnits.Tests

open Swensen.Unquote
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Informedica.GenUnits.Lib


/// Create the necessary test generators
module Generators =

    let bigRGen (n, d) = 
            let d = if d = 0 then 1 else d
            let n' = abs(n) |> BigRational.FromInt
            let d' = abs(d) |> BigRational.FromInt
            n'/d'

    let bigRGenerator =
        gen {
            let! n = Arb.generate<int>
            let! d = Arb.generate<int>
            return bigRGen(n, d)
        }

    type MyGenerators () =
        static member BigRational () =
            { new Arbitrary<BigRational>() with
                override x.Generator = bigRGenerator }


[<SetUpFixture>]
type Config () =
    
    /// Make sure the generators are
    /// registered before running any
    /// test code.
    [<SetUp>]
    member x.Setup () = 

        Arb.register<Generators.MyGenerators>() |> ignore

module UnitTests =
    
        module NameTests =
            
            module N = Informedica.GenUnits.Lib.Unit.Name     
            
            let create = N.create       

            type ``Given an empty or null string`` () =                    
                
                [<Test>]
                member x.``Name cannot be created`` () =
                    let succ _ = false
                    let fail _ = true
                    let create = create succ fail
                    test <@ create ""  @>
                    test <@ create null @>

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
