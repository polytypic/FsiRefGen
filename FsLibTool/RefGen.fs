module RefGen

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text
open System.Text.RegularExpressions

let group r = "(?:" + r + ")"
let many r = r + "*"
let choice rs = String.concat "|" rs |> group
let strings ss = Array.map Regex.Escape ss |> choice
let chars (s: string) = s.ToCharArray () |> Array.map string |> strings

let keywords =
  [|":>"; "->"; "="; "[<"; ">]"; "(|" ; "|)"; "[|"; "|]"; "?<-"; "??"; "?";
    ":?>"; ":?"; ":>"; "::"; ":="; ":";
    "abstract"; "and";
    "class";
    "default"; "do!"; "downto"; "do";
    "else"; "end"; "exception";
    "false"; "finally"; "for"; "fun"; "function";
    "if"; "inherit"; "inline"; "in"; "interface";
    "let!"; "let";
    "match"; "member"; "module"; "mutable";
    "namespace"; "new";
    "of"; "open"; "override";
    "rec"; "return!"; "return";
    "static"; "struct";
    "then"; "to"; "true"; "try"; "type";
    "upcast"; "use!"; "use";
    "val";
    "when"; "while"; "with";
    "yield!"; "yield";
    |]

let keywordSet = HashSet<_>(keywords)
let punctuation =
  [|","; "("; ")"; "{"; "}"; "["; "]"; "'"; "#"; ":"; ";;"; ";"|]
let nonIdSet = HashSet<_>(Array.concat [|keywords; punctuation; [|"."|]|])

let id = group ("[a-zA-Z_]" + many "[a-zA-Z0-9_']")
let num = group ("[0-9]" + many "[0-9.sxuylLfF]")
let symFirst = "!%&*+-./<=>@^|~"
let symRest = symFirst + "?"
let sym = group (chars symFirst + many (chars symRest))
let str = "\\\"(?:[^\\\\\"]|\\\\.)*\\\""
let char = "\\'.*\\'"

let tokenRegex =
  let extra = Array.concat [|keywords; punctuation|] |> strings
  Regex (choice [|id; extra; sym; num; str; char|])

let tokenize (s: string) =
  let rec loop ts = function
   | [] -> []
   | [m: Match] -> List.rev ((m.Value, 0)::ts)
   | m::n::ms ->
     loop ((m.Value, n.Index - m.Index - m.Value.Length)::ts) (n::ms)
  tokenRegex.Matches s |> Seq.cast |> List.ofSeq |> loop []

let (|T|) (t, _) = t

let (|Attr|_|) ts =
  match ts with
   | (T"[<" as attr)::ts ->
     let rec loop attrs = function
      | [] -> failwith "Invalid attr or attr spans multiple lines."
      | (T">]" as attr)::ts -> Some (List.rev (attr::attrs), ts)
      | attr::ts -> loop (attr::attrs) ts
     loop [attr] ts
   | _ -> None

let (|Id|_|) ts =
  let rec loop is = function
   | (i, 0)::(".", 0)::ts when not (nonIdSet.Contains i) ->
     loop (i::is) ts
   | (i, n)::ts when not (nonIdSet.Contains i) ->
     Some (i, n, is, ts)
   | _ ->
     None
  loop [] ts

let summarize s =
  tokenize s |> List.filter (function T"inline" -> false | _ -> true)

let indentOf (s: String) =
  let rec loop i = if i < s.Length && s.[i] = ' ' then loop (i+1) else i
  loop 0

let trim s = (indentOf s, s.Trim ())

let input (file: string) =
  File.ReadAllLines file |> Array.map trim |> List.ofArray

type Item =
 {Doc: list<string>
  Attr: list<list<string * int>>
  Kind: option<string>
  Id: option<string>
  Path: list<string>
  Indent: int
  Tokens: list<string * int>
  Body: list<Item>}
let empty =
 {Doc = []
  Attr = []
  Kind = None
  Id = None
  Path = []
  Indent = 0
  Tokens = []
  Body = []}

let rec itemize ls =
  let rec outside indent path items docs attrs = function
     | [] -> (List.rev items, [])
     | (i, text: string)::lines ->
       if text.Length = 0
          || List.exists text.StartsWith ["#"; "// "; "open "] then
         outside indent path items docs attrs lines
       elif i <= indent then
         (List.rev items, (i, text)::lines)
       elif text.StartsWith "////" then
         let item = {Doc = [text.Substring 4]
                     Attr = []
                     Kind = Some "spacer"
                     Id = None
                     Path = []
                     Indent = i
                     Tokens = []
                     Body = []}
         outside indent path (item::items) [] [] lines
       elif text.StartsWith "///" then
         outside indent path items (text.Substring 3::docs) attrs lines
       elif text.StartsWith "//# " then
         let item = {Doc = [text.Substring 4]
                     Attr = []
                     Kind = Some "header"
                     Id = None
                     Path = []
                     Indent = i
                     Tokens = []
                     Body = []}
         outside indent path (item::items) [] [] lines
       else
         let nest path docs attrs kind id indent' tokens =
            let (body, lines) = outside indent' (id::path) [] [] [] lines
            let item = {Doc = List.rev docs
                        Attr = List.rev attrs
                        Kind = Some kind
                        Id = Some id
                        Path = path
                        Indent = i
                        Tokens = tokens
                        Body = body}
            outside indent path (item::items) [] [] lines
         let tokens = summarize text
         match tokens with
          | T("namespace"|"module" as kind)::Id (id, _, path, []) ->
            nest path docs attrs kind id indent tokens
          | T("type" as kind) as t::Attr (attr, (T id::_ as ts)) ->
            nest path docs (attr::attrs) kind id i (t::ts)
          | T"and"::Attr (attr, (T id::_)) ->
            nest path docs (attr::attrs) "type" id i tokens
          | T("exception" as kind) as t::(T id::_ as ts) ->
            nest path docs attrs kind id i (t::ts)
          | T"static"::T"member"::T id::T":"::_ ->
            nest path docs attrs "static member" id i tokens
          | T"static"::T"member"::T "("::T id::T ")"::T":"::_ ->
            nest path docs attrs "static member" id i tokens
          | T"and"::T id::([]|T"<"::_|T":>"::_|T"="::_|T"with"::_) ->
            nest path docs attrs "type" id i tokens
          | T("val" as kind)::T"mutable"::T id::T":"::_
          | T("val" as kind)::T"("::T id::T")"::T":"::_
          | T("val" as kind)::T"(|"::T id::T"|)"::T":"::_
          | T("module"|"namespace" as kind)::T id::[T"="]
          | T("abstract"|"default"|"member"|"val" as kind)::T id::(T":"::_|T"<"::_)
          | T("type" as kind)::T id::([]|T"<"::_|T":>"::_|T"="::_|T"with"::_)
          | T("new" as id as kind)::T":"::_ ->
            nest path docs attrs kind id i tokens
          | T("mutable")::T id::T":"::_
          | T id::T":"::_ ->
            nest path docs attrs "field" id i tokens
          | Attr (attr, []) ->
            outside indent path items docs (attr::attrs) lines
          | T("{"|"}"|"->"|"*"|"inherit"|"interface"|"end"|"|")::_ ->
            outside indent path ({empty with Tokens = tokens; Indent = i}::items) [] [] lines
          | _ ->
            failwithf "Unrecognized: %A" tokens
  outside -1 [] [] [] [] ls |> fst

let esc (b: StringBuilder) (s: string) =
  for i=0 to s.Length-1 do
    match s.[i] with
     | '<' -> b.Append "&lt;"  |> ignore
     | '>' -> b.Append "&gt;"  |> ignore
     | '&' -> b.Append "&amp;" |> ignore
     | c   -> b.Append c |> ignore

let asText (s: string) =
  let b = StringBuilder ()
  esc b s
  b.ToString ()

let linkName id path kind =
   let prefix = match kind with None -> "" | Some kind -> kind + " "
   asText (prefix + String.concat "." (List.rev (id::path)))

let spaceRegex = Regex " "
let escapeSpaces s = spaceRegex.Replace (s, "%20")

let printTokens wr id2items space spaced inSection toSection linkId path kind ts =
  let linked = ref (Option.isNone linkId)
  let spaces n =
    if n = 0 then ""
    elif spaced then String.replicate n space
    else space
  let rec loop = function
   | (k, n)::ts when keywordSet.Contains k ->
     if k = ":" || k = "->" then linked := true
     fprintf wr "%s%s" (asText k) (spaces n) ; loop ts
   | ("'", _)::(t, n)::ts ->
     fprintf wr "'%s%s" (asText t) (spaces n) ; loop ts
   | Id (id, n, fullIdPath, ts)
       when Option.isSome (Map.tryFind id id2items)
         && (id <> "unit" || id = "unit" && not (!linked)) ->
     
     let rec parts id idPath ts =
       if not (!linked) && linkId = Some id then
         linked := true

         match idPath with
          | [] -> ()
          | id::idPath ->
            parts id idPath [(".", 0)]
            fprintf wr "."

         let text = asText id
         let link = linkName id path kind
         let id = match inSection with
                   | None -> " "
                   | Some inSection -> sprintf " id=\"%s:%s\" " inSection link
         fprintf wr "<a%shref=\"#%s:%s\">%s</a>" id toSection (escapeSpaces link) text
       else
         let kindsFilter =
           match ts with
            | T"<"::_ -> fun item -> item.Kind = Some "type"
            | T"."::_ -> fun item -> item.Kind = Some "module"
            | _ -> fun _ -> true

         let rec cmp = function
          | ([], _) -> true
          | (_, []) -> true
          | (p1::p1s, p2::p2s) ->
            p1 = p2 && cmp (p1s, p2s)

         let pathFilter (item: Item) =
           match idPath with
            | [] ->
              //printf "%A %A\n" path item.Path
              let rec paths = function
               | [] -> false
               | (_::rest) as path ->
                 path = item.Path || paths rest
              paths path
            | _ ->
              cmp (idPath, item.Path)

         let filterIfNotUnique pred xs =
           match xs with
            | [_] -> xs
            | _ -> List.filter pred xs

         let items =
           match Map.tryFind id id2items with
            | None -> []
            | Some items ->
              items
              |> filterIfNotUnique kindsFilter
              |> filterIfNotUnique pathFilter
              |> List.sortBy (fun item -> item.Kind)

         let items =
           match items with
            | [i1; i2] when i1.Kind = Some "module" && i2.Kind = Some "type" -> [i2]
            | _ -> items

         match idPath with
          | [] -> ()
          | id::idPath ->
            parts id idPath [(".", 0)]
            fprintf wr "."

         let isUnique item =
           match item.Id with
            | Some id -> 
              match Map.tryFind id id2items with
               | Some [_] -> true
               | _ -> false
            | None -> false

         match items with
          | [item] when isUnique item || pathFilter item ->
            let link = linkName id item.Path item.Kind
            fprintf wr "<a href=\"#%s:%s\">%s</a>"
             "def" (escapeSpaces link) (asText id)
          | _ ->
            let longId = String.concat "." (List.rev (id::idPath))
            printf "Failed to resolve: %s\n" longId
            fprintf wr "%s" (asText id)
     
     parts id fullIdPath ts
     fprintf wr "%s" (spaces n)
     loop ts
   | (s, n)::ts when Array.exists ((=) s.[0]) (symFirst.ToCharArray ()) ->
     fprintf wr "%s%s" (asText s) (spaces n) ; loop ts
   | (t, n)::ts ->
     fprintf wr "%s%s" (asText t) (spaces n) ; loop ts
   | [] ->
     ()
  loop ts

let printDesc wr item id2items (lines: list<string>) =
  let path =
    match (item.Path, item.Id) with
     | ([], Some id) -> [id]
     | (path, _) -> path
  let rec paras = function
    | (empty: string)::lines when "" = empty.Trim () ->
      paras lines
    | code::lines when code.StartsWith ">" ->
      fprintf wr "<pre><code class=\"fsharp hljs\">"
      inPre (code::lines)
    | text::lines ->
      fprintf wr "<p>"
      inPara 0 (text::lines)
    | [] -> ()
  and inPre = function
    | text::lines when text.StartsWith "> " ->
      let code = text.Substring 2
      fprintf wr "%s" (String.replicate (indentOf code) " ")
      printTokens wr id2items " " true None "def" None path None (tokenize code)
      fprintf wr "\n"
      inPre lines
    | lines ->
      fprintf wr "</code></pre>\n"
      paras lines
  and inPara i = function
    | (empty: string)::lines when i=0 && "" = empty.Trim () ->
      fprintf wr "</p>\n"
      paras lines
    | code::lines when i=0 && code.StartsWith "> " ->
      fprintf wr "</p>\n<pre><code class=\"fsharp hljs\">"
      inPre (code::lines)
    | (text: string)::lines when i < text.Length ->
      match text.[i] with
       | '`' ->
         fprintf wr """<code class="fsharp hljs">"""
         inCode text.[i] (i+1) (i+1) (text::lines)
       | c ->
         fprintf wr "%c" c
         inPara (i+1) (text::lines)
    | text::lines when i = text.Length ->
      inPara 0 lines
    | lines ->
      fprintf wr "</p>\n"
      paras lines
  and inCode esc i0 i = function
    | (text : string)::lines when i < text.Length ->
      if esc = text.[i] then
        printTokens wr id2items "&nbsp;" true None "def" None path None (tokenize (text.Substring (i0, i-i0)))
        fprintf wr "</code>"
        inPara (i+1) (text::lines)
      else
        inCode esc i0 (i+1) (text::lines)
    | text::lines when i = text.Length ->
      printTokens wr id2items "&nbsp;" true None "def" None item.Path None (tokenize (text.Substring (i0, i-i0)))
      fprintf wr "&nbsp;"
      inCode esc 0 0 lines
    | lines ->
      failwithf "Failed to escape text properly: %A" lines
  paras lines

let hasDocs item =
  not <| List.isEmpty item.Doc

let rec bodyHasDocs item =
  item.Body
  |> List.exists (fun item ->
     hasDocs item || bodyHasDocs item)

let isObsolete (item: Item) =
  item.Attr
  |> List.exists (List.exists (fst >> (=) "Obsolete"))

let needsSummary item =
  not (isObsolete item) && (hasDocs item || bodyHasDocs item)

let rec printSummary wr id2items deep inSection toSection drop item =
  let indent = max (item.Indent - drop) 0
  let prefix = String.replicate indent " "
  if item.Kind = Some "header" then
    fprintfn wr """%s<span class="h3">// %s</span>""" prefix (List.head item.Doc |> asText)
  elif item.Kind = Some "spacer" then
    fprintf wr "</code></pre>\n<pre><code class=\"fsharp hljs\">"
  else
    let spacing = deep && (item.Kind = Some "module" || item.Kind = Some "type")
    if spacing then
      fprintf wr """<span class="spacing">"""
    item.Attr
    |> Seq.iter ^ fun attr ->
         fprintf wr "%s" prefix
         printTokens wr id2items " " false inSection toSection None [] None attr
         fprintf wr "\n"
    fprintf wr "%s" prefix
    printTokens wr id2items " " deep inSection toSection
     (match item.Doc with [] -> None | _ -> item.Id) item.Path item.Kind item.Tokens
    fprintf wr "\n"
    if deep then
      item.Body
      |> Seq.filter (isObsolete >> not)
      |> Seq.iter (printSummary wr id2items deep inSection toSection drop)
    else
      if not (List.exists (fun item -> Option.isSome item.Id) item.Body) then
        item.Body
        |> Seq.filter ^ fun item ->
             match item.Tokens with
              | T("{"|"}")::_ -> false
              | _ -> true
        |> Seq.iter (printSummary wr id2items false inSection toSection drop)
    if spacing then
      fprintf wr "</span>"

let rec printDescription wr id2items item =
  if Option.isSome item.Id &&
     (not (List.isEmpty item.Doc) ||
      not (List.isEmpty item.Body)) then
    if needsSummary item then
      fprintf wr "<div class=\"description\"><pre><code class=\"fsharp hljs\">"
      printSummary wr id2items false (Some "def") "dec" item.Indent item
      fprintf wr "</code></pre>\n"
      match item.Doc with
       | [] -> ()
       | lines -> printDesc wr item id2items lines
      if item.Body |> List.exists (fun item -> List.isEmpty item.Doc |> not)
      then item.Body
           |> Seq.iter (printDescription wr id2items)
      fprintf wr "</div>\n"

let generate wr title path =
  let units =
    Directory.EnumerateFiles (path, "*.fsi")
    |> Seq.map (input >> itemize)
    |> List.ofSeq
  let model =
    let collect field =
      units
      |> List.collect (List.collect field)
    match units with
     | (unit::_)::_ ->
       {unit with
         Doc = collect ^ fun item -> item.Doc
         Attr = collect ^ fun item -> item.Attr
         Body = collect ^ fun item -> item.Body}
     | _ -> failwith "Expected some!"
  let id2items =
    let rec addItems id2items item =
      match item.Id with
       | None ->
         id2items
       | Some id ->
         let items =
           match Map.tryFind id id2items with
            | Some items -> items
            | None -> []
         let id2items =
           match item.Doc with
            | [] -> id2items
            | _ -> Map.add id (item::items) id2items
         List.fold
          addItems
          id2items
          item.Body
    addItems Map.empty model
  fprintf wr "%s" """<!doctype html>
<html>
<head>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/normalize/4.1.1/normalize.css">
<link rel="stylesheet" href="fs-libtool.css">
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.4.0/styles/github.min.css">
<script src="fs-libtool.js" async></script>
"""
  fprintfn wr "<title>%s Library Reference</title>" title
  fprintfn wr "%s" """</head>
<body><table width="80%" align="center"><tr><td>"""
  fprintfn wr "<h1>%s Library Reference</h1>" title
  fprintfn wr "<h2>Synopsis</h2>"
  fprintf wr "<pre><code class=\"fsharp hljs\">"
  printTokens wr id2items " " false (Some "dec") "def" model.Id model.Path model.Kind model.Tokens
  fprintf wr "\n"
  model.Body
  |> Seq.filter (isObsolete >> not)
  |> Seq.iter ^ printSummary wr id2items true (Some "dec") "def" 0
  fprintf wr "</code></pre>\n"
  fprintf wr "<h2>Description</h2>\n"
  printDescription wr id2items model
  fprintf wr "</td></tr></table></body>\n"
  fprintf wr "</html>\n"
