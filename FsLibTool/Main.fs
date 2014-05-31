open System
open System.IO

type LibInfo = {
    LibDir: string
    LibName: string
    DocFile: string
  }

let genDocs lib =
  printfn "Generating reference for %s." lib.LibName
  use wr = new StreamWriter (lib.DocFile)
  RefGen.generate wr lib.LibName lib.LibDir

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
  for lib in libs do
    genDocs lib
  0
