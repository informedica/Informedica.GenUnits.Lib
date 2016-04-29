#I "./../packages/Newtonsoft.Json/lib/net45/"
#r "Newtonsoft.Json.dll"
open Newtonsoft.Json


type Project = 
    {
        repositoryName: string
        repositoryBranch : string
        updated: System.DateTime
    }


type Build = 
    {
       buildNumber: int
       branch: string
       commitId : string
       status : string
       started: System.DateTime
    }

type Result = 
    {
        project: Project
        build: Build
    }


let getLatestBuild project =
    try
        let url = sprintf "https://ci.appveyor.com/api/projects/%s/%s" "halcwb" project
        let wr = System.Net.HttpWebRequest.Create(url) :?> System.Net.HttpWebRequest
        wr.Accept <- "application/json"
        let resp = wr.GetResponse()
        use reader = new System.IO.StreamReader(resp.GetResponseStream())
        let json = reader.ReadToEnd()
        let build = JsonConvert.DeserializeObject<Result>(json)
        (build.build.commitId, build.build.started, build.build.branch, build.build.status) |> Some
    with
    | e -> printfn "%A" e; None

