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
    |> List.ofArray
    
let pairRules =
    lines
    |> Array.skip 2
    |> Array.map (fun s -> s.Split(" -> ") |> (fun x -> (x[0][0], x[0][1]), x[1][0]))
    |> Map
    
let mapPairToInsertion pair =
    pairRules
    |> Map.find pair
    
let updateCharCounts counts char =
    Map.change char (fun x ->
        match x with
        | Some count -> Some (count + 1UL)
        | None -> Some 1UL
        ) counts

let mergeMaps m1 m2 =
    (Map.toList m1) @ (Map.toList m2)
    |> List.groupBy fst
    |> List.map (fun (char, countList) -> (char, List.sumBy (fun x -> snd x) countList))
    |> Map

let charCountAfterSteps steps startingTemplate =
    let cache = new Dictionary<string,Map<char,uint64>>()
    
    let rec countChars steps currentStep pair =
        //$"start: current step: {currentStep}, pair: {pair}".Dump()
        
        let cacheKey = $"{fst pair}{snd pair}{currentStep}"
        
        if cache.ContainsKey(cacheKey) then
            cache[cacheKey]
        else
            let mappedChar = mapPairToInsertion pair
            
            let result =
                if currentStep <= steps then
                    let left = countChars steps (currentStep + 1) (fst pair, mappedChar)
                    let right = countChars steps (currentStep + 1) (mappedChar, snd pair)
                    
                    let merged = mergeMaps left right
                    
                    updateCharCounts merged mappedChar
                else
                    Map [ (mappedChar, 1UL) ]
            
            //$"end: current step: {currentStep}, pair: {pair}, result: {result}".Dump()
            cache.Add(cacheKey, result)
            
            result
    
    let initialCount = startingTemplate |> List.countBy id |> List.map (fun (char, cnt) -> (char, uint64 cnt)) |> Map
    if steps > 0 then
        startingTemplate
        |> List.pairwise
        |> List.fold (fun s p -> mergeMaps s (countChars (steps - 1) 1 p)) Map.empty<char,uint64>
        |> (fun c -> mergeMaps c initialCount)
    else
        initialCount

let sw = Stopwatch.StartNew()

let result =
    charCountAfterSteps 40 polymerTemplate
    |> Map.values
    |> (fun charCounts -> (Seq.max charCounts), (Seq.min charCounts))
    |> (fun (maxChars, minChars) -> maxChars - minChars)

result.Dump()

sw.Stop()
sw.Elapsed.Dump("Time")
