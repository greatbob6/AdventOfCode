<Query Kind="FSharpProgram" />

let dumper x = x.Dump()
let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

let generateDiff l =
    l
    |> List.windowed 2
    |> List.map (fun v -> v[1] - v[0])
    
let rec getDiffList all l =
    let diffs = generateDiff l
    
    if List.forall (fun x -> x = 0) diffs then
        diffs :: all
    else
        getDiffList (diffs :: all) diffs
        
let rec addValueToLists valToAdd resultLists lists =
    let list = List.head lists
    let newList = List.append list [ (List.last list) + valToAdd ]
    
    match lists with
    | [ _ ] -> (newList :: resultLists)
    | _ -> addValueToLists (List.last newList) (newList :: resultLists) (List.tail lists)
    
let getNextVal (vals: string) =
    vals
    |> (fun s -> s.Split(" ") |> Array.map int |> Array.toList)
    |> (fun l -> getDiffList [l] l)
    |> addValueToLists 0 []
    |> List.head |> List.last

lines
|> Array.map getNextVal
|> Array.sum
|> dumper
