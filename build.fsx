// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#I "packages/FAKE/tools/"

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open Fake.UserInputHelper
open System
open System.IO
#if MONO
#else
#load "packages/SourceLink.Fake/tools/Fake.fsx"
open SourceLink
#endif

#load "scripts/Travis.fsx"
#load "scripts/AppVeyor.fsx"
#load "scripts/Settings.fsx"

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = Settings.project

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = Settings.summary

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = Settings.description

// List of author names (for NuGet package)
let authors = Settings.authors

// Tags for your project (for NuGet package)
let tags = Settings.tags

// File system information 
let solutionFile  = Settings.solutionFile

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = Settings.testAssemblies

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = Settings.gitOwner 
let gitHome = Settings.gitHome

// The name of the project on GitHub
let gitName = Settings.gitName

let reposName = Settings.reposName

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" ("https://raw.github.com/" + gitOwner + "/" + reposName)

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|) (projFileName:string) = 
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)


let getCurrentBranch () = (getGitResult "" "rev-parse --abbrev-ref HEAD").[0]


// --------------------------------------------------------------------------------------
// Integrate current branch with master

Target "Integrate" (fun _ ->
    let master   = "master"
    let remote   = "origin"
    let develop  = getCurrentBranch()
    let currDir  = __SOURCE_DIRECTORY__
    let traVsucc = "passed"
    let appVsucc = "success"
    let currId   = Git.Information.getCurrentSHA1 currDir   

    printfn "Starting to integrate"
    if Git.Information.isCleanWorkingCopy currDir |> not then 
        failwith "Working copy is not clean, cannot integrate"
    else
        // Get the latest mono build
        let lastTravisBuild   = Travis.getLatestBuild gitName
        // Get the latest .Net build
        let lastAppVeyorBuild = AppVeyor.getLatestBuild gitName

        // Check whether the builds passed
        match lastTravisBuild, lastAppVeyorBuild with
        | Some (id, dt, br, st), Some (id', dt', br', st') -> 
            if id = currId && id = id' && st = traVsucc && st' = appVsucc then
                printfn "Last build was at %A and %s" dt st
                // Update the master branch with the latest remote master
                Git.Branches.checkoutBranch currDir master
                Git.Branches.pull "" remote master
                // Merge the master with the current development branch
                Git.Merge.merge currDir Git.Merge.FastForwardFlag develop
                // Update the remote master branch
                Git.Branches.pushBranch currDir remote master
                // Checkout the current development branch
                Git.Branches.checkoutBranch currDir develop
            else 
                printfn "curr: %s, travis: %s, appveyor; %s" currId id id'
                sprintf "Last build did not pass on %s" (if st = traVsucc then "AppVeyor" else "Travis")
                |> failwith
        | e   -> 
            failwith <| sprintf "Cannot get last build, cannot integrate: %A" e
)



// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title (projectName)
          Attribute.Product project
          Attribute.Company (authors |> String.concat ", ")
          Attribute.Description summary
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath, 
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.map getProjectDetails
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (folderName @@ "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName @@ "Properties") @@ "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName @@ "My Project") @@ "AssemblyInfo.vb") attributes
        )
)

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the 
// src folder to support multiple project outputs
Target "CopyBinaries" (fun _ ->
    !! "src/**/*.??proj"
    |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) @@ "bin/Release", "bin" @@ (System.IO.Path.GetFileNameWithoutExtension f)))
    |>  Seq.iter (fun (fromDir, toDir) -> CopyDir toDir fromDir (fun _ -> true))
)

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs ["bin"; "temp"]
)

Target "CleanDocs" (fun _ ->
    CleanDirs ["docs/output"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    if solutionFile <> "" then
        !! solutionFile
        |> MSBuildRelease "" "Rebuild"
        |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    match !! testAssemblies with
    | tests when tests |> Seq.length > 0 ->
        Console.ForegroundColor <- ConsoleColor.Cyan
        tests
        |> NUnit (fun p ->
            { p with
                DisableShadowCopy = true
                TimeOut = TimeSpan.FromMinutes 20.
                OutputFile = "TestResults.xml" })
        Console.ForegroundColor <- ConsoleColor.White
    | _ -> ()
)

#if MONO
#else
// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries http://ctaggart.github.io/SourceLink/

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw project
    !! "src/**/*.??proj"
    |> Seq.iter (fun projFile ->
        let proj = VsProj.LoadRelease projFile 
        SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb __SOURCE_DIRECTORY__ baseUrl
    )
)

#endif

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->
    Paket.Pack(fun p -> 
        { p with
            OutputPath = "bin"
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes})
)

Target "PublishNuget" (fun _ ->
    let key = getBuildParam "NugetKey"
    Paket.Push(fun p -> 
        { p with
            WorkingDir = "bin"
            ApiKey = key })
)


// --------------------------------------------------------------------------------------
// Generate the documentation

Target "GenerateReferenceDocs" (fun _ ->
    if not <| executeFSIWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"; "--define:REFERENCE"] [] then
      failwith "generating reference documentation failed"
)

let generateHelp' fail debug =
    let args =
        if debug then ["--define:HELP"]
        else ["--define:RELEASE"; "--define:HELP"]
    if executeFSIWithArgs "docs/tools" "generate.fsx" args [] then
        traceImportant "Help generated"
    else
        if fail then
            failwith "generating help documentation failed"
        else
            traceImportant "generating help documentation failed"

let generateHelp fail =
    generateHelp' fail false

Target "GenerateHelp" (fun _ ->
    let releasenotes = "docs/content/release-notes.md"
    if fileExists releasenotes then DeleteFile releasenotes
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"
    let license = "docs/content/license.md"
    if fileExists license then DeleteFile license
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp true
)

Target "GenerateHelpDebug" (fun _ ->
    DeleteFile "docs/content/release-notes.md"
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    DeleteFile "docs/content/license.md"
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp' true true
)

Target "KeepRunning" (fun _ ->    
    use watcher = !! "docs/content/**/*.*" |> WatchChanges (fun changes ->
         generateHelp false
    )

    traceImportant "Waiting for help edits. Press any key to stop."

    System.Console.ReadKey() |> ignore

    watcher.Dispose()
)

Target "GenerateDocs" DoNothing

let createIndexFsx lang =
    let content = """(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../../bin"

(**
F# Project Scaffold ({0})
=========================
*)
"""
    let targetDir = "docs/content" @@ lang
    let targetFile = targetDir @@ "index.fsx"
    ensureDirectory targetDir
    System.IO.File.WriteAllText(targetFile, System.String.Format(content, lang))

Target "AddLangDocs" (fun _ ->
    let args = System.Environment.GetCommandLineArgs()
    if args.Length < 4 then
        failwith "Language not specified."

    args.[3..]
    |> Seq.iter (fun lang ->
        if lang.Length <> 2 && lang.Length <> 3 then
            failwithf "Language must be 2 or 3 characters (ex. 'de', 'fr', 'ja', 'gsw', etc.): %s" lang

        let templateFileName = "template.cshtml"
        let templateDir = "docs/tools/templates"
        let langTemplateDir = templateDir @@ lang
        let langTemplateFileName = langTemplateDir @@ templateFileName

        if System.IO.File.Exists(langTemplateFileName) then
            failwithf "Documents for specified language '%s' have already been added." lang

        ensureDirectory langTemplateDir
        Copy langTemplateDir [ templateDir @@ templateFileName ]

        createIndexFsx lang)
)

// --------------------------------------------------------------------------------------
// Release Scripts


Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    
    if Branches.getAllBranches "" |> List.exists ((=) "remotes/origin/gh-pages") then
        Repository.cloneSingleBranch "" (gitHome + "/" + gitName) "gh-pages" tempDocsDir

        CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
        StageAll tempDocsDir

        Git.Commit.Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
        Branches.push tempDocsDir
    else
        printfn "No gh-pages branch, going to create one"
        let currentBranch = getCurrentBranch()
        // Create a clean gh-pages branch
        gitCommand "" "checkout --orphan gh-pages" |> tracefn "%A"
        gitCommand "" "rm -rf ." |> tracefn "%A"
        // Add the index file to enable a push of the branch
        CreateFile "index.html"
        StageFile "" "index.html" |> tracefn "%A"
        Git.Commit.Commit "" "Added gh-pages branch" |> tracefn "%A"
        Branches.pushBranch "" "origin" "gh-pages" |> tracefn "%A"
        // Restore the current branch
        Branches.checkoutBranch "" currentBranch
        gitCommand "rm -f index.html" |> ignore
        failwith "Run ReleaseDocs again"
)


#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "Release" (fun _ ->
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + reposName))
        |> function None -> gitHome + "/" + reposName | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion
    
    // release on github
    createClient user pw
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
    // TODO: |> uploadFile "PATH_TO_FILE"    
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "BuildPackage" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override


Target "All" DoNothing


"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "RunTests"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"
  ==> "All"
  =?> ("ReleaseDocs",isLocalBuild)

"All" 
#if MONO
#else
  =?> ("SourceLink", Pdbstr.tryFind().IsSome )
#endif
  ==> "NuGet"
  ==> "BuildPackage"

"CleanDocs"
  ==> "GenerateHelp"
  ==> "GenerateReferenceDocs"
  ==> "GenerateDocs"

"CleanDocs"
  ==> "GenerateHelpDebug"

"GenerateHelp"
  ==> "KeepRunning"
    
"ReleaseDocs"
  ==> "Release"

"BuildPackage"
  ==> "PublishNuget"
  ==> "Release"


RunTargetOrDefault "All"
