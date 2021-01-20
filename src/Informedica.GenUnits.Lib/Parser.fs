namespace Informedica.GenUnits.Lib

module Parser =

    open MathNet.Numerics

    open Informedica.Utils.Lib.BCL

    open FParsec
    open ValueUnit


    let setUnitValue u v =
        u
        |> apply (fun _ -> v)


    let ws =
        anyOf [' '; '\t'; '\n']
        |> many


    let pUnit =
        Units.units
        |> List.collect (fun ud ->
            [
                ud.Abbreviation.Dut, fun n -> n |> setUnitValue ud.Unit
                ud.Abbreviation.Eng, fun n -> n |> setUnitValue ud.Unit
                ud.Name.Dut, fun n -> n |> setUnitValue ud.Unit
                ud.Name.Eng, fun n -> n |> setUnitValue ud.Unit
                yield! (ud.Synonyms |> List.map (fun s -> s, fun n -> n |> setUnitValue ud.Unit))
            ]
        )
        |> List.distinctBy fst
        |> List.sortByDescending (fun (s, _) -> s, s |> String.length)
        |> List.map (fun (s, u) -> s |> String.toLower |> pstring >>% u)
        |> choice
        |> fun p ->
            opt pfloat
            .>> ws
            .>>. p
            |>> (fun (f, u) -> f |> Option.bind BigRational.fromFloat, u)


    let pCombiUnit =  sepBy1 pUnit (ws >>. (pchar '/') .>> ws)


    let pValueUnit =
        (opt pfloat)
        |>> (Option.bind BigRational.fromFloat)
        .>> ws
        .>>. pCombiUnit
        .>> ws


    let parse s =
        if s |> String.isNullOrWhiteSpace then None
        else
            s
            |> String.trim
            |> String.replace "," "."
            |> String.toLower
            |> run pValueUnit
            |> function
            | Success (result, _, _) ->
                let v, us = result
                match v with
                | Some br ->
                    us
                    |> List.map (fun (vo, tu) ->
                        match vo with
                        | Some v -> v  |> tu
                        | None   -> 1N |> tu
                    )
                    |> List.rev
                    |> List.reduce per
                    |> ValueUnit.create
                    |> fun f -> f br
                    |> Some
                | None -> None
            | Failure (msg, _, _) ->
                printfn "parsing failure: %s" msg
                None
