namespace Informedica.GenUnits.Lib


module StringBCL =

    let toString s = s.ToString()

    let empty (s: string) = System.String.IsNullOrWhiteSpace(s)

    let notEmpty = empty >> not

    let toUpper (s: string) = s.ToUpper()

    let toLower (s: string) = s.ToLower()

    let length (s: string) = s.Length

    let substring start length (s: string) = s.Substring(start, length)

    let trim (s: string) = s.Trim()

    /// Get the first character of a string
    /// as a string
    let firstStringChar = substring 0 1

    /// Return the rest of a string as a string
    let restString s = substring 1 ((s |> length) - 1) s

    let firstToUpper = firstStringChar >> toUpper

    let capitalize s = (s |> firstToUpper) + (s |> restString |> toLower)

    let letters = ['a'..'z'] @ ['A'..'Z'] |> List.map toString

    let isLetter s = List.exists (fun s' -> s' = s) letters

    let equalsCapInsens s1 s2 = s1 |> toLower |> trim = (s2 |> toLower |> trim) 


module Utils =  
    
    module Nullable =
    
        let isNull x = x = Unchecked.defaultof<_>
        let isNotNull x = x |> isNull |> not

        let (|NotNull|IsNull|) x = 
            if x |> isNull then IsNull else NotNull


