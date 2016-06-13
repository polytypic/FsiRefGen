open System
open System.IO

let exePath = Reflection.Assembly.GetExecutingAssembly().Location

type Options = {
  out: option<string>
  name: option<string>
  icon: option<string>
  files: list<string>
}

let help () =
  printfn "Usage: %s --out <output-dir> --name <name> [--icon iconUrl] -- <input.fsi> ..."
    ^ Path.GetFileName exePath
  1

let go options =
  match options with
   | {out = Some out
      name = Some name
      icon = icon
      files = files} when not ^ List.isEmpty files ->
     Directory.CreateDirectory out |> ignore

     let exeDir = Path.GetDirectoryName exePath
     for file in ["fs-libtool.js"; "fs-libtool.css"] do
       File.Copy (Path.Combine (exeDir, file), Path.Combine (out, file), true)

     let docFile = Path.Combine (out, name + ".html")

     use wr = new StreamWriter (docFile)
     RefGen.generate wr name icon files

     0
   | _ ->
     help ()

let rec parseOptions options args =
  match args with
   | "--out" :: dir :: args ->
     parseOptions {options with out = Some dir} args
   | "--name" :: name :: args ->
     parseOptions {options with name = Some name} args
   | "--icon" :: icon :: args ->
     parseOptions {options with icon = Some icon} args
   | "--" :: args ->
     go {options with files = args}
   | _ ->
     help ()

[<EntryPoint>]
let main argv =
  List.ofArray argv
  |> parseOptions {out = None; name = None; icon = None; files = []}
