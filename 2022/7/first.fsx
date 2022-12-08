type File = {
    Name: string
    Size: int
}

type Directory = {
    Name: string
    Directories: Directory list
    Files: File list
}

let getDirectoryFilesSize dir =
    System.Console.WriteLine(dir.Files)
    List.sumBy (fun f -> f.Size) dir.Files

let getDirectorySize dir =
    let rec getDirSize dir : int=
        let fileSize = getDirectoryFilesSize dir

        let subDirsSize =
            dir.Directories
            |> List.map getDirSize
            |> List.sum

        fileSize + subDirsSize

    getDirSize dir

type FileSystemEntry =
    | DirectoryInfo of string
    | FileInfo of string * int

type Operations =
    | ChangeDirectory of string
    | List of FileSystemEntry list

let parseFileSystemEntry (str: string) : FileSystemEntry =
    System.Console.WriteLine(str)
    if str.StartsWith("dir") then
        DirectoryInfo (str.Substring(4))
    else
        let matches = System.Text.RegularExpressions.Regex.Match(str, "([0-9+]) (.+)")

        FileInfo (matches.Groups[2].Value, int(matches.Groups[1].Value))

let parseLines (lines: string list) =
    let rec parse ops (strs: string list) =
        match strs with
        | str :: rest ->
            System.Console.WriteLine($"Parse: {str}")
            if str.StartsWith("$ cd") then
                parse ((ChangeDirectory (str.Substring(5))) :: ops) rest
            elif str.StartsWith("$ ls") then
                let entries =
                    rest
                    |> List.takeWhile (fun x -> not(x.StartsWith("$")))
                    |> List.map parseFileSystemEntry
                
                parse ((List entries) :: ops) (rest[(List.length entries)..])
            else
                parse ops rest
        | [] -> ops 

    parse [] lines

(*
let processOp (state: Map<string list, FileSystemEntry list>) (curPath: string list) op =
    match op with
    | ChangeDirectory dir ->
        if curPath[0] <> dir then
            (state, dir :: curPath)
        else
            (state, curPath)
    | List -> (state, curPath)
    | FileSystemEntry e ->
        let newState = Map.add curPath 
*)

let ops =
    System.IO.File.ReadAllLines("7/test.txt")
    |> List.ofArray
    |> parseLines
