<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

let lines = 
    getData "puzzle"

let polymerTemplate =
    lines
    |> Array.head
    |> (fun s -> s.ToCharArray())
    
let pairRules =
    lines
    |> Array.skip 2
    |> Array.map (fun s -> s.Split(" -> ") |> (fun x -> (x[0].ToCharArray(), x[1].ToCharArray()[0])))
    
let mapPairToInsertion idx pair =
    let rule =
        pairRules
        |> Array.tryFind (fun (rulePair, _) -> rulePair = pair)
    
    match rule with
    | None -> None
    | Some r ->
        Some (idx + 1, snd r)

//polymerTemplate.Dump()
//pairRules.Dump()

let step template =
    let inserts =
        template
        |> Array.pairwise
        |> Array.map (fun (c1, c2) -> [| c1; c2 |])
        |> Array.mapi mapPairToInsertion
        |> Array.choose id
        |> Array.sortByDescending fst
        
    inserts
    |> Array.fold (fun temp (idx, char) -> Array.insertAt idx char temp) template
    
let result =
    seq { 1..10 }
    |> Seq.fold (fun s _ -> step s) polymerTemplate
    |> Array.countBy id
    |> (fun charCounts -> (Array.maxBy snd charCounts), (Array.minBy snd charCounts))
    |> (fun (maxChars, minChars) -> (snd maxChars) - (snd minChars))

result.Dump()
