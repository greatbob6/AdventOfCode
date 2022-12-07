type File = {
    Name: string
    Size: int
}

type Directory = {
    Name: string
    Directories: Directory list
    Files: File list
}

let getDirectoryFileSize dir = List.sumBy (fun f -> f.Size) dir.Files

let getDirectorySize dir =
    let rec getDirSize total dir =
        let fileSize = getDirectoryFileSize dir

        dir.Directories
        |> List.map (getDirSize (total + fileSize))
        |> List.sum

    getDirSize 0 dir

