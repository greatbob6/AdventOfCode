<Query Kind="FSharpProgram" />

type Line = {
    X1: int
    Y1: int
    X2: int
    Y2: int
}

let parseLine (str: string) =
    let pattern = "(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)"
    let matched = Regex.Match(str, pattern)
    
    {
        X1 = int matched.Groups["x1"].Value
        Y1 = int matched.Groups["y1"].Value
        X2 = int matched.Groups["x2"].Value
        Y2 = int matched.Groups["y2"].Value
    }

let (|InVertical|InHorizontal|InDiagonal|NoMatch|) (x, y, l) =
    let inVertical = (x = l.X1 && x = l.X2 && y >= Math.Min(l.Y1, l.Y2) && y <= Math.Max(l.Y1, l.Y2))
    let inHorizontal = (y = l.Y1 && y = l.Y2 && x >= Math.Min(l.X1, l.X2) && x <= Math.Max(l.X1, l.X2))
    
    if inVertical then InVertical
    elif inHorizontal then InHorizontal
    else
        let isPointInBounds = x >= (min l.X1 l.X2) && x <= (max l.X1 l.X2) && y >= (min l.Y1 l.Y2) && y <= (max l.Y1 l.Y2)
        
        if isPointInBounds then
            let m = (l.Y2 - l.Y1) / (l.X2 - l.X1)
            let b = l.Y1 - (m * l.X1)
            
            let isPointOnLine = (y = (m * x + b))
        
            if isPointOnLine then InDiagonal else NoMatch

        else NoMatch

let isPointInLineSegment (x, y) l =
    match (x, y, l) with
    | InVertical | InHorizontal | InDiagonal -> true
    | _ -> false

let lines = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\5\puzzle_input.txt")
    |> Array.map parseLine

let maxX = lines |> Array.map (fun l -> max l.X1 l.X2) |> Array.max
let maxY = lines |> Array.map (fun l -> max l.Y1 l.Y2) |> Array.max

let xRange = [0..maxX]
let yRange = [0..maxY]

let pointList =
    Seq.allPairs [0..maxX] [0..maxY]
    |> Seq.map (fun p -> Array.filter (isPointInLineSegment p) lines |> Array.length)
    |> Seq.filter (fun overlap -> overlap >= 2)
    |> Seq.length

pointList.Dump()