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
  let root = if argv.Length = 0 then "Libs" else argv.[0]
  let target = if argv.Length <= 1 then "Docs/gh-pages" else argv.[1]

  let libs =
    Directory.EnumerateDirectories root
    |> Seq.filter (fun libDir ->
       Directory.EnumerateFiles (libDir, "*.fsi")
       |> Seq.isEmpty |> not)
    |> Seq.map (fun libDir ->
       let libName = Path.GetFileName libDir
       let docFile = Path.Combine(target, libName + ".html")
       {LibDir = libDir
        LibName = libName
        DocFile = docFile})
    |> List.ofSeq
  for lib in libs do
    genDocs lib
  0
