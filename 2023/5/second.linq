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

let rangeOverlaps r1 r2 = r1.SeedStart <= r2.SeedEnd && r1.SeedEnd >= r2.SeedStart
let combineRanges r1 r2 = { SeedStart = min r1.SeedStart r2.SeedStart; SeedEnd = max r1.SeedEnd r2.SeedEnd }

let seedRangeProcessor listOfMappings currentMin seedRange =
    let seedCount = seedRange.SeedEnd - seedRange.SeedStart
    seedCount.Dump("Seed Count")
    
    let sw = Stopwatch.StartNew()
    
    let minLocation = min currentMin (getMinLocationForSeedRange listOfMappings seedRange)
    
    sw.Stop()
    
    sw.Elapsed.Dump("SeedRange Processing Time")
    (sw.ElapsedMilliseconds / seedCount).Dump("Ms per seed")
    
    minLocation

let sections = splitList [] lines

let seedRanges = sections[0][0] |> parseSeedRanges
let listOfMappings = List.map (fun (l: string list) -> l.Tail |> parseMappings) sections.Tail

(*
seedRanges
|> Array.fold (seedRangeProcessor listOfMappings) Int64.MaxValue
|> (fun x -> x.Dump())
*)

seedRanges
|> Array.Parallel.map (fun seedRange -> getMinLocationForSeedRange listOfMappings seedRange)
|> Array.min
|> (fun x -> x.Dump())
