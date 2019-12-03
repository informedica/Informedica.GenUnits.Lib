// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()

Target.create "Clean" <| fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs 


Target.create "Build" <|fun _ ->
    !! "src/**/*.*proj"
    |> Seq.iter (DotNet.build id)


Target.create "All" ignore

Target.create "Tests" <| fun _ ->
    !! "tests/**/*.*proj"
    |> Seq.iter (fun p ->
        DotNet.exec id "run" ("--project " + p)
        |> ignore
    )

"Clean"
  ==> "Build"
  ==> "Tests"
  ==> "All"

Target.runOrDefault "All"

