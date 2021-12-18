<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

let lines = 
    getData "test"

let polymerTemplate =
    lines
    |> Array.head
    |> (fun s -> s.ToCharArray())
    
let pairRules =
    lines
    |> Array.skip 2
    |> Array.map (fun s -> s.Split(" -> ") |> (fun x -> (x[0].ToCharArray(), x[1].ToCharArray()[0])))
    |> Map
    
let mapPairToInsertion idx (p1, p2) =
    let pairArray = [| p1; p2 |]
    
    pairRules
    |> Map.find pairArray
    |> (fun r -> (idx + 1, r))

let step template stepNum =
    $"Processing step: {stepNum}".Dump()
    let sw = Stopwatch.StartNew()
    
    let inserts =
        template
        |> Array.pairwise
        |> Array.mapi mapPairToInsertion
        
    let result =
        template
        |> Array.foldBack (fun (idx, char) temp -> Array.insertAt idx char temp) inserts
    
    sw.Stop()
    $"  Completed step: {stepNum} in {sw.Elapsed}".Dump()
    
    result
    
let result =
    seq { 1..40 }
    |> Seq.fold step polymerTemplate
    |> Array.countBy id
    |> (fun charCounts -> (Array.maxBy snd charCounts), (Array.minBy snd charCounts))
    |> (fun (maxChars, minChars) -> (snd maxChars) - (snd minChars))

result.Dump()
