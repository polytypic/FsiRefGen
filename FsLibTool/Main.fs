open System
open System.IO

type LibInfo = {
    LibDir: string
    LibName: string
    DocFile: string
  }

[<EntryPoint>]
let main argv =
  let libs =
    Directory.EnumerateDirectories "Libs"
    |> Seq.filter (fun libDir ->
       Directory.EnumerateFiles (libDir, "*.fsi")
       |> Seq.isEmpty |> not)
    |> Seq.map (fun libDir ->
       let libName = Path.GetFileName libDir
       let docFile = Path.Combine ("Docs", libName + ".html")
       {LibDir = libDir
        LibName = libName
        DocFile = docFile})
    |> List.ofSeq
  printf "%A" libs
  0
