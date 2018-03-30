#r @"packages/build/FAKE/tools/FakeLib.dll"

open System

open Fake
// TODO-NMB?... open Fake.Azure.Kudu

let serverDir = "./src/server" |> FullName
let uiDir = "./src/ui" |> FullName
let publishDir = "./publish" |> FullName

let dotnetcliVersion = DotNetCli.GetDotNetSDKVersionFromGlobalJson ()

let mutable dotnetExePath = "dotnet"

let run' timeout cmd args dir =
    if not (execProcess (fun info ->
        info.FileName <- cmd
        info.UseShellExecute <- cmd.Contains "yarn" && Console.OutputEncoding <> Text.Encoding.GetEncoding(850)
        if not (String.IsNullOrWhiteSpace dir) then info.WorkingDirectory <- dir
        info.Arguments <- args
    ) timeout) then failwithf "Error while running '%s' with args: %s" cmd args

let run = run' System.TimeSpan.MaxValue

let runDotnet workingDir args =
    let result = ExecProcess (fun info ->
        info.FileName <- dotnetExePath
        info.WorkingDirectory <- workingDir
        info.Arguments <- args) TimeSpan.MaxValue
    if result <> 0 then failwithf "dotnet %s failed" args

let platformTool tool winTool = (if isUnix then tool else winTool) |> ProcessHelper.tryFindFileOnPath |> function Some t -> t | _ -> failwithf "%s not found" tool

let nodeTool = platformTool "node" "node.exe"
let yarnTool = platformTool "yarn" "yarn.cmd"

let ipAddress = "localhost"
let port = 8080

do if not isWindows then
    // We have to set the FrameworkPathOverride so that dotnet SDK invocations know where to look for full-framework base class libraries.
    let mono = platformTool "mono" "mono"
    let frameworkPath = IO.Path.GetDirectoryName (mono) </> ".." </> "lib" </> "mono" </> "4.5"
    setEnvironVar "FrameworkPathOverride" frameworkPath

Target "install-dot-net-core" (fun _ -> dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion)

Target "clean" (fun _ ->
    CleanDir (serverDir </> "bin")
    DeleteFiles !! @".\src\server\obj\*.nuspec"
    CleanDir (uiDir </> "bin")
    DeleteFiles !! @".\src\ui\obj\*.nuspec"
    CleanDir (uiDir </> "public")
    CleanDir publishDir)

Target "copy-resources" (fun _ ->
    let publicResourcesDir = uiDir </> @"public\resources"
    CreateDir publicResourcesDir
    CopyFiles publicResourcesDir !! @".\src\resources\images\*.*")

Target "install-server" (fun _ -> runDotnet serverDir "restore")

Target "install-ui" (fun _ ->
    printfn "Node version:"
    run nodeTool "--version" __SOURCE_DIRECTORY__
    printfn "Yarn version:"
    run yarnTool "--version" __SOURCE_DIRECTORY__
    run yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__
    runDotnet uiDir "restore")

Target "install" DoNothing

Target "build" (fun _ ->
    runDotnet serverDir "build"
    runDotnet uiDir "fable webpack -- -p")

Target "run" (fun _ ->
    let server = async { runDotnet serverDir "watch run" }
    let ui = async { runDotnet uiDir "fable webpack-dev-server" }
    let openBrowser = async {
        do! Async.Sleep 5000
        Diagnostics.Process.Start (sprintf "http://%s:%d" ipAddress port) |> ignore }
    Async.Parallel [| server ; ui ; openBrowser |] |> Async.RunSynchronously |> ignore)

Target "publish" (fun _ ->
    CreateDir publishDir
    let serverDir = publishDir </> "server"
    CreateDir serverDir
    CopyFiles serverDir !! @".\src\server\bin\Debug\netcoreapp2.0\*.*"
    let uiDir = publishDir </> "ui"
    CreateDir uiDir
    CopyFile uiDir @".\src\ui\index.html"
    let publicDir = uiDir </> "public"
    CreateDir publicDir
    let publicJsDir = publicDir </> "js"
    CreateDir publicJsDir
    CopyFiles publicJsDir !! @".\src\ui\public\js\*.js"
    let publicStyleDir = publicDir </> "style"
    CreateDir publicStyleDir
    CopyFiles publicStyleDir !! @".\src\ui\public\style\*.css" 
    let publicResourcesDir = publicDir </> "resources"
    CreateDir publicResourcesDir
    CopyFiles publicResourcesDir !! @".\src\ui\public\resources\*.*")

Target "help" (fun _ ->
    printfn "\nThe following build targets are defined:"
    printfn "\n\tbuild ... builds server and ui [which writes output to .\\src\\ui\\public]"
    printfn "\tbuild run ... builds and runs server and ui [using webpack dev-server]"
    printfn "\tbuild publish ... builds server and ui, then copies output to .\\publish\n")

"install-dot-net-core" ==> "install-server" ==> "install"
"install-dot-net-core" ==> "install-ui" ==> "install"
"clean" ==> "install-server"
"clean" ==> "copy-resources" ==> "install-ui"
"install" ==> "build" ==> "publish"
"install" ==> "run"

RunTargetOrDefault "build"
