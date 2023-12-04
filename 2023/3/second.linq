<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt") |> Array.toList


let numParser = new Regex("[0-9]+", RegexOptions.Compiled)
let symParser = new Regex("[^0-9.]", RegexOptions.Compiled)

type ParsedNum = {
    Value: int
    StartIndex: int
    EndIndex: int
    Y: int
}

type BoundedNum = {
    Value: int
    MinX: int
    MaxX: int
    MinY: int
    MaxY: int
}

type Symbol = {
    Value: string
    X: int
    Y: int
}

let parseNumbersFromLine y (s: string) =
    numParser.Matches(s)
    |> Seq.map (fun x -> x.Groups[0].Value, x.Groups[0].Index, x.Groups[0].Length)
    |> Seq.map (fun (v, i, l) -> { Value = int v; StartIndex = i; EndIndex = i + l - 1; Y = y })
    |> Seq.toList
    
let parsedNumToBoundedNum (pn: ParsedNum) =
    { Value = pn.Value; MinX = pn.StartIndex - 1; MaxX = pn.EndIndex + 1; MinY = pn.Y - 1; MaxY = pn.Y + 1 }

let parseSymbolsFromLine y (s: string) =
    symParser.Matches(s)
    |> Seq.map (fun x -> { Value = x.Groups[0].Value; X = x.Groups[0].Index; Y = y })
    |> Seq.toList
    
let overlaps (bn: BoundedNum) (s: Symbol) = s.X >= bn.MinX && s.X <= bn.MaxX && s.Y >= bn.MinY && s.Y <= bn.MaxY

let checkBoundedNumOverlapsSymbol bn symbols =
    symbols
    |> List.exists (overlaps bn)

let nums =
    lines
    |> List.mapi parseNumbersFromLine
    |> List.collect id
    |> List.map parsedNumToBoundedNum    

let syms =
    lines
    |> List.mapi parseSymbolsFromLine
    |> List.collect id
    
let gears =
    syms
    |> List.filter (fun x -> x.Value = "*")
    |> List.map (fun x -> List.filter (fun n -> overlaps n x) nums)
    |> List.filter (fun l -> List.length l = 2)
    
gears
|> List.map (fun l -> l |> List.map (fun x -> x.Value) |> List.reduce (*))
|> List.sum
|> (fun x -> x.Dump())
