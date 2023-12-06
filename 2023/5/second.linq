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
    
type SeedRange = {
    SeedStart: int64
    SeedEnd: int64
}

let parseSeedRanges (s: string) =
    s.Substring(7).Split(" ")
    |> Array.map int64
    |> Array.chunkBySize 2
    |> Array.map (fun a -> { SeedStart = a[0]; SeedEnd = a[0] + a[1] - 1L })

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
    
let getMinLocationForSeedRange listOfMappings seedRange =
    seq { seedRange.SeedStart .. seedRange.SeedEnd }
    |> Seq.fold (fun currentMin seedNum -> min currentMin (getLocationForSeed listOfMappings seedNum)) Int64.MaxValue

let sections = splitList [] lines

let seedRanges = sections[0][0] |> parseSeedRanges
let listOfMappings = List.map (fun (l: string list) -> l.Tail |> parseMappings) sections.Tail

seedRanges
|> Array.map (fun sr -> (sr, Array.exists (fun isr -> sr.SeedStart <= isr.SeedEnd && sr.SeedEnd >= isr.SeedStart) seedRanges))
|> (fun x -> x.Dump())

// Seed Ranges overlap...need to deduplicate the ranges

(*
seedRanges
|> Array.fold (fun currentMin seedRange -> min currentMin (getMinLocationForSeedRange listOfMappings seedRange)) Int64.MaxValue
|> (fun x -> x.Dump())
*)
