// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket:
nuget FSharp.Core
nuget Fake.DotNet.Cli
nuget Fake.DotNet.Paket
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.IO.FileSystem
nuget Fake.Core.Target
nuget Fake.Core.ReleaseNotes
//"

#load "./.fake/build.fsx/intellisense.fsx"

//Temporary fix until this is resolved : https://github.com/mono/mono/issues/9315
#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif
//Temporary fix until this is resolved : https://github.com/mono/mono/issues/9315


System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet


// Utils

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|) (projFileName:string) = 
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)



// Properties
let buildDir = "./build/"
let project = "src/Informedica.GenUnits.Lib/Informedica.GenUnits.Lib.fsproj"
let summary = "A library for calculation with units"
let release = ReleaseNotes.load "RELEASE_NOTES.md"
let srcGlob = "*.fsproj"
let distDir = "./dist"

// Targets

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" <| fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ AssemblyInfo.Title (projectName)
          AssemblyInfo.Product project
          AssemblyInfo.Description summary
          AssemblyInfo.Version release.AssemblyVersion
          AssemblyInfo.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath, 
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
        match projFileName with
        | Fsproj -> AssemblyInfoFile.createFSharp (folderName @@ "AssemblyInfo.fs") attributes
        | Csproj -> AssemblyInfoFile.createCSharp ((folderName @@ "Properties") @@ "AssemblyInfo.cs") attributes
        | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName @@ "My Project") @@ "AssemblyInfo.vb") attributes
        )



Target.create "clean" <| fun _ ->
    Trace.trace "Cleaning up stuff..."
    Shell.cleanDir buildDir


Target.create "build" <| fun _ ->
    Trace.trace "Build the project..."
    DotNet.build id project  


Target.create "test" <| fun _ ->
    Trace.trace "Running tests..."

    let cmd = "run"
    let args = "--project tests/Informedica.GenUnits.Tests/Informedica.GenUnits.Tests.fsproj"
    let result = 
        DotNet.exec 
            (fun x -> { x with DotNetCliPath = "dotnet" }) 
            cmd 
            args
    if not result.OK then 
        failwithf "`dotnet %s %s` failed" cmd args


Target.create "bundle" <| fun _ ->
    !! project
    |> Seq.iter (fun proj ->
        let args =
            [
                sprintf "/p:PackageVersion=%s" release.NugetVersion
                sprintf "/p:PackageReleaseNotes=\"%s\"" (release.Notes |> String.concat "\n")
            ] |> String.concat " "
        DotNet.pack (fun c ->
            { c with
                Configuration = DotNet.BuildConfiguration.Release
                OutputPath = Some distDir
                Common =
                    c.Common
                    |> DotNet.Options.withCustomParams (Some args)
            }) proj
    )


Target.create "nuget" <| fun _ ->
    Paket.pack(fun p -> 
        printf "NuGet: %A" p
        { p with
            OutputPath = "bin"
            Version = release.NugetVersion
            ReleaseNotes = String.toLines release.Notes})


Target.create "publishnuget" <| fun _ ->
    Paket.push(fun p -> 
        { p with
            WorkingDir = "bin" })



Target.create "nothing" ignore


// Dependencies

open Fake.Core.TargetOperators


"clean"
==> "build"
==> "test"
==> "bundle"


// Start build

Target.runOrDefault "build"


