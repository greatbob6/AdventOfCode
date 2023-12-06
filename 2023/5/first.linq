<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt") |> Array.toList

let rec splitList (ll: string list list) (l: string list) =
    let idx = List.tryFindIndex (fun x -> x = String.Empty) l
    
    match idx with
    | Some i ->
        let lists = List.splitAt i l
        
        splitList ((fst lists) :: ll) (snd lists).Tail
    | None -> List.rev (l :: ll)

type Mapping = {
    DestinationStart: int64
    DestinationEnd: int64
    SourceStart: int64
    SourceEnd: int64
}

let mappingFromString (s: string) =
    let parts = s.Split(" ")
    
    let destinationStart = parts[0] |> int64
    let sourceStart = parts[1] |> int64
    let len = parts[2] |> int64
    
    { DestinationStart = destinationStart; DestinationEnd = destinationStart + len - 1L; SourceStart = sourceStart; SourceEnd = sourceStart + len - 1L }

let parseMappings (entries: string list) =
    entries
    |> List.map mappingFromString
    
let applyMappings (mappings: Mapping list) (input: Int64) =
    let mapping = List.tryFind (fun x -> input >= x.SourceStart && input <= x.SourceEnd) mappings
    
    match mapping with
    | None -> input
    | Some m -> input - m.SourceStart + m.DestinationStart

let getLocationForSeed listOfMappings seedNum =
    listOfMappings
    |> List.fold (fun v mappings -> applyMappings mappings v) seedNum

let sections = splitList [] lines

let seedList = sections[0][0] |> (fun x -> x.Substring(7).Split(" ") |> Array.map int64)

let listOfMappings = List.map (fun (l: string list) -> l.Tail |> parseMappings) sections.Tail

seedList
|> Array.map (getLocationForSeed listOfMappings)
|> Array.min
|> (fun x -> x.Dump())