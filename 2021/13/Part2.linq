<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

type Fold =
| Up of int
| Left of int

let foldRegex = new Regex("fold along ([x|y])=(\d+)", RegexOptions.Compiled)

let parseFold s =
    let m = foldRegex.Match(s)
    
    if m.Success then
        if m.Groups[1].Value = "y" then
            Up (int m.Groups[2].Value)
        else
            Left (int m.Groups[2].Value)
    else
        failwith "BROKEN!"

let combineHalves (first: bool[,]) (second: bool[,]) =
    first
    |> Array2D.mapi (fun y x v -> v || (second[y,x]))
        
let flipVertical (data: bool[,]) =
    seq { 0 .. (Array2D.length1 data)-1 }
    |> Seq.map (fun r -> data[r, *])
    |> Seq.rev
    |> array2D

let foldUp row (data: bool[,]) =
    let top = data[..row-1, *]
    let bottom =
        data[row+1..,*]
        |> flipVertical
    
    combineHalves top bottom
    
let flipHorizontal (data: bool[,]) =
    seq { 0 .. (Array2D.length2 data)-1 }
    |> Seq.map (fun c -> data[*, c])
    |> Seq.rev
    |> Seq.transpose
    |> array2D

let foldLeft column (data: bool[,]) =
    let left = data[*, ..column-1]
    let right =
        data[*,column+1..]
        |> flipHorizontal
    
    combineHalves left right
    
let applyFold (data: bool[,]) fold =
    match fold with
    | Up y -> foldUp y data
    | Left x -> foldLeft x data

let lines = 
    getData "puzzle"

let splitIndex =
    lines
    |> Array.findIndex (fun s -> String.IsNullOrWhiteSpace(s))

let points =
    lines[0..splitIndex-1]
    |> Array.map (fun s -> s.Split(","))
    |> Array.map (fun sa -> ((int sa[0]), (int sa[1])))

let folds =
    lines[splitIndex+1..]
    |> Array.map parseFold

let (maxX, maxY) =
    points
    |> Array.fold (fun (curMaxX,curMaxY) (x,y) -> (max curMaxX x), (max curMaxY y)) (0, 0)
    |> (fun (x, y) -> (x + (x % 2), y + (y % 2)))

let paper =
    Array2D.create (maxY + 1) (maxX + 1) false
    |> Array2D.mapi (fun y x _ -> if Array.contains (x,y) points then true else false)

let folded =
    folds
    //|> Array.take 1
    |> Array.fold applyFold paper
    
folded.Dump()

let total =
    Seq.allPairs [0..(Array2D.length2 folded)-1] [0..(Array2D.length1 folded)-1]
    |> Seq.fold (fun total (x,y) -> if folded[y,x] then total + 1 else total) 0
    
total.Dump()