#r @"packages/build/FAKE/tools/FakeLib.dll"

open System

open Fake

let serverPath = "./src/server" |> FullName
let uiPath = "./src/ui" |> FullName
let deployDir = "./deploy" |> FullName

let platformTool tool winTool =
  let tool = if isUnix then tool else winTool
  tool
  |> ProcessHelper.tryFindFileOnPath
  |> function Some t -> t | _ -> failwithf "%s not found" tool

let nodeTool = platformTool "node" "node.exe"
let yarnTool = platformTool "yarn" "yarn.cmd"

let dotnetcliVersion = DotNetCli.GetDotNetSDKVersionFromGlobalJson()
let mutable dotnetCli = "dotnet"

let run cmd args workingDir =
  let result =
    ExecProcess (fun info ->
      info.FileName <- cmd
      info.WorkingDirectory <- workingDir
      info.Arguments <- args) TimeSpan.MaxValue
  if result <> 0 then failwithf "'%s %s' failed" cmd args

Target "Clean" (fun _ -> 
  CleanDirs [deployDir]
)

Target "InstallDotNetCore" (fun _ ->
  dotnetCli <- DotNetCli.InstallDotNetSDK dotnetcliVersion
)

Target "InstallUI" (fun _ ->
  printfn "Node version:"
  run nodeTool "--version" __SOURCE_DIRECTORY__
  printfn "Yarn version:"
  run yarnTool "--version" __SOURCE_DIRECTORY__
  run yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__
  run dotnetCli "restore" uiPath
)

Target "RestoreServer" (fun () -> 
  run dotnetCli "restore" serverPath
)

Target "Build" (fun () ->
  run dotnetCli "build" serverPath
  run dotnetCli "fable webpack -- -p" uiPath
)

Target "Run" (fun () ->
  let server = async {
    run dotnetCli "watch run" serverPath
  }
  let ui = async {
    run dotnetCli "fable webpack-dev-server" uiPath
  }
  let browser = async {
    Threading.Thread.Sleep 5000
    Diagnostics.Process.Start "http://localhost:8080" |> ignore
  }

  [ server; ui; browser]
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore
)


"Clean"
  ==> "InstallDotNetCore"
  ==> "InstallUI"
  ==> "Build"

"InstallUI"
  ==> "RestoreServer"
  ==> "Run"

RunTargetOrDefault "Build"
