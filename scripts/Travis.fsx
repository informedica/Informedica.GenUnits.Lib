#I "./../packages/Newtonsoft.Json/lib/net45/"
#r "Newtonsoft.Json.dll"

open System
open Newtonsoft.Json

[<CLIMutable>]
type Build = 
    {
      commit_id: Nullable<int>
      number: string
      duration: Nullable<int>
      finished_at: Nullable<DateTime>
      started_at: Nullable<DateTime>
      state: string
    }

type Commit = 
  {
    id: Nullable<int> 
    sha: string
    branch: string
    message: string
    committed_at: string
  }

[<CLIMutable>]
type Builds =
    {
        builds: Build[]
        commits: Commit[]
    }

let getLatestBuild project =
  try
    let url = sprintf "http://api.travis-ci.org/repos/halcwb/%s/builds" project
    let wr = System.Net.HttpWebRequest.Create(url) :?> System.Net.HttpWebRequest
    wr.Host <- "api.travis-ci.org"
    wr.UserAgent <- project
    wr.Accept <- "application/vnd.travis-ci.2+json"
    let resp = wr.GetResponse()
    use reader = new System.IO.StreamReader(resp.GetResponseStream())
    let json = reader.ReadToEnd()
//    printfn "%A" json
    let builds = JsonConvert.DeserializeObject<Builds>(json)
    (builds.commits.[0].sha, builds.builds.[0].started_at, builds.commits.[0].branch, builds.builds.[0].state) |> Some
  with 
  | e -> printfn "Failed with %A" e; None


