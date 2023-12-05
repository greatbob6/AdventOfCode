<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt") |> Array.toList

type Card = {
    Number: int
    WinningNumbers: int list
    CollectedNumbers: int list
}

let parseCard (s: string) =
    let split1 = s.Split(":") |> Array.map (fun x -> x.Trim())
    
    let cardNum = split1[0].Substring(5) |> int
    
    let numsBlocks = split1[1].Split("|") |> Array.map (fun x -> x.Trim())
    
    let winningNums = numsBlocks[0].Split(" ") |> Array.map (fun x -> x.Trim()) |> Array.filter (fun x -> x.Length > 0) |> Array.map int |> Array.toList
    
    let collectedNums = numsBlocks[1].Split(" ") |> Array.map (fun x -> x.Trim()) |> Array.filter (fun x -> x.Length > 0) |> Array.map int |> Array.toList
    
    { Number = cardNum; WinningNumbers = winningNums; CollectedNumbers = collectedNums }
    
let getCardScore c =
    let matches = c.CollectedNumbers |> List.filter (fun x -> List.contains x c.WinningNumbers)
    let matchCount = List.length matches
    
    if matchCount = 0 then 0 else pown 2 (matchCount - 1)

lines
|> List.map parseCard
|> List.map getCardScore
|> List.sum
|> (fun x -> x.Dump())
