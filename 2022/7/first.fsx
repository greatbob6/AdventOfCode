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

type State = {
    CurrentPath: string list
    Files: Map<string list, File list>
}

let foldChangeDir state dir =
    if dir = ".." then
        { state with CurrentPath = List.tail state.CurrentPath }
    elif List.head state.CurrentPath = dir then
        state
    else
        { state with CurrentPath = dir :: state.CurrentPath }

let foldList state items =
    let addedFiles =
        items
        |> List.choose (fun x -> match x with | FileInfo (name, size) -> Some { Name = name; Size = size } | _ -> None)

    let newMap =
        if Map.containsKey state.CurrentPath state.Files then
            let curFiles = Map.find state.CurrentPath state.Files
            let newFiles = List.append curFiles addedFiles

            Map.change state.CurrentPath (fun _ -> Some newFiles) state.Files
        else
            Map.add state.CurrentPath addedFiles state.Files

    { state with Files = newMap }

let folder (state: State) op =
    match op with
    | ChangeDirectory dir -> foldChangeDir state dir
    | List items -> foldList state items

let generateFileMap ops =
    let s =
        ops
        |> List.fold folder { CurrentPath = [ "/" ]; Files = Map.empty<string list, File list> }

    s.Files

let fileMapToList filemap =
    filemap
    |> Map.toList
    |> List.map fst
    |> List.map List.rev

let result =
    System.IO.File.ReadAllLines("7/test.txt")
    |> List.ofArray
    |> parseLines
    |> List.rev
    |> generateFileMap
    |> fileMapToList
