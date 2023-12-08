<Query Kind="FSharpProgram" />

let dumper x = x.Dump()
let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")
    
let cardValue c =
    match c with
    | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> int64 (string c)
    | 'T' -> 10L
    | 'J' -> 11L
    | 'Q' -> 12L
    | 'K' -> 13L
    | 'A' -> 14L
    
let calcCardsValue (cards: char array) =
    let groupedCards =
        cards
        |> Array.groupBy id
        |> Array.map (fun (_, grp) -> Array.length grp)
        |> Array.sortDescending
        
    let handValue =
        if groupedCards[0] = 5 then
            70L
        else if groupedCards[0] = 4 then
            60L
        else if groupedCards[0] = 3 && groupedCards[1] = 2 then
            50L
        else if groupedCards[0] = 3 then
            40L
        else if groupedCards[0] = 2 && groupedCards[1] = 2 then
            30L
        else if groupedCards[0] = 2 then
            20L
        else
            10L
    
    (handValue * 10000000000L) + ((cardValue cards[0]) * 100000000L) + ((cardValue cards[1]) * 1000000L) + ((cardValue cards[2]) * 10000L) + ((cardValue cards[3]) * 100L) + (cardValue cards[4])
    
let rec compareByCards (c1: char array) (c2: char array) idx =
    let val1 = cardValue c1[idx]
    let val2 = cardValue c2[idx]
    
    //$"{c1[idx]} = {val1}, {c2[idx]} = {val2}".Dump()
    
    if val1 = val2 then compareByCards c1 c2 (idx + 1)
    else val1.CompareTo(val2)

[<CustomComparison; StructuralEquality>]
type Hand =
    {
        Cards: char array
        Bid: int
        Rank: int
    }
    
    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Hand as h -> (this :> IComparable<_>).CompareTo h
            | _ -> -1

    interface IComparable<Hand> with
        member this.CompareTo other =
            let otherVal = calcCardsValue other.Cards
            let thisVal = calcCardsValue this.Cards
            
            if thisVal = otherVal then compareByCards this.Cards other.Cards 0
            else thisVal.CompareTo otherVal

lines
|> Array.map (fun s -> s.Split(" "))
|> Array.map (fun a -> { Cards = a[0].ToCharArray(); Bid = int a[1]; Rank = 0 })
|> Array.sortBy (fun h -> calcCardsValue h.Cards)
|> Array.mapi (fun i h -> { h with Rank = i + 1 })
|> Array.map (fun h -> h.Bid * h.Rank)
|> Array.sum
|> dumper

(*
compareByCards [| 'T'; 'A'; '4'; 'J'; '3' |] [| 'T'; '2'; 'J'; '5'; 'A' |] 0
|> dumper
*)