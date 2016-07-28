open System
open System.IO

let exePath = Reflection.Assembly.GetExecutingAssembly().Location

type Options = {
  out: option<string>
  name: option<string>
  icon: option<string>
  version: option<string>
  projectUrl: option<string>
  files: list<string>
}

let help () =
  printfn "Usage: %s --out <output-dir> --name <name> [--icon iconUrl] -- <input.fsi> ..."
    ^ Path.GetFileName exePath
  1

let go = function
  | {out = Some out
     name = Some name
     icon = icon
     version = version
     projectUrl = projectUrl
     files = files} when not ^ List.isEmpty files ->
    Directory.CreateDirectory out |> ignore

    let exeDir = Path.GetDirectoryName exePath
    for file in ["fsirefgen.js"; "fsirefgen.css"] do
      File.Copy (Path.Combine (exeDir, file), Path.Combine (out, file), true)

    let docFile = Path.Combine (out, name + ".html")

    use wr = new StreamWriter (docFile)
    RefGen.generate wr
      {name=name
       icon=icon
       version=version
       projectUrl=projectUrl
       files=files}

    0
  | _ ->
    help ()

let rec parseOptions options = function
  | "--out" :: dir :: args ->
    parseOptions {options with out = Some dir} args
  | "--name" :: name :: args ->
    parseOptions {options with name = Some name} args
  | "--icon" :: icon :: args ->
    parseOptions {options with icon = Some icon} args
  | "--project-url" :: projectUrl :: args ->
    parseOptions {options with projectUrl = Some projectUrl} args
  | "--version" :: version :: args ->
    parseOptions {options with version = Some version} args
  | "--" :: args ->
    go {options with files = args}
  | _ ->
    help ()

[<EntryPoint>]
let main argv =
  List.ofArray argv
  |> parseOptions {out = None
                   name = None
                   icon = None
                   version = None
                   projectUrl = None
                   files = []}
