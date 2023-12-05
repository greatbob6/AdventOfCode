<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt") |> Array.toList

type Card = {
    Number: int
    Matches: int
    TotalCards: int
}

let parseCard (s: string) =
    let split1 = s.Split(":") |> Array.map (fun x -> x.Trim())
    
    let cardNum = split1[0].Substring(5) |> int
    
    let numsBlocks = split1[1].Split("|") |> Array.map (fun x -> x.Trim())
    
    let winningNums = numsBlocks[0].Split(" ") |> Array.map (fun x -> x.Trim()) |> Array.filter (fun x -> x.Length > 0) |> Array.map int |> Array.toList
    
    let collectedNums = numsBlocks[1].Split(" ") |> Array.map (fun x -> x.Trim()) |> Array.filter (fun x -> x.Length > 0) |> Array.map int |> Array.toList
    
    { Number = cardNum; Matches = collectedNums |> List.filter (fun x -> List.contains x winningNums) |> List.length; TotalCards = 1 }

    
let processCard (cards: Card array) (c: Card) =
    seq { 1 .. c.Matches }
    |> Seq.iter (fun n ->
        let idx = c.Number - 1 + n
        cards[idx] <- { cards[idx] with TotalCards = cards[idx].TotalCards + c.TotalCards }
    )
    
let cards =
    lines
    |> List.map parseCard
    |> List.toArray

cards
|> Array.iter (processCard cards)

cards
|> Array.map (fun c -> c.TotalCards)
|> Array.sum
|> (fun x -> x.Dump())
