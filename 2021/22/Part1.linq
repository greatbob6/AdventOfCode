<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

type OnOff = On | Off

type InitStep =
    {
        Action: OnOff
        Xmin: int
        Xmax: int
        Ymin: int
        Ymax: int
        Zmin: int
        Zmax: int
    }

let parser = new Regex("(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)", RegexOptions.Compiled)

let parseInitStep line =
    let matches = parser.Match(line)
    
    {
        Action = if matches.Groups[1].Value = "on" then On else Off
        Xmin = int matches.Groups[2].Value
        Xmax = int matches.Groups[3].Value
        Ymin = int matches.Groups[4].Value
        Ymax = int matches.Groups[5].Value
        Zmin = int matches.Groups[6].Value
        Zmax = int matches.Groups[7].Value
    }

let coordSeqFromStep step =
    let xOutsideBounds = (step.Xmin < -50 && step.Xmax < -50) || (step.Xmin > 50 && step.Xmax > 50)
    let yOutsideBounds = (step.Ymin < -50 && step.Ymax < -50) || (step.Ymin > 50 && step.Ymax > 50)
    let zOutsideBounds = (step.Zmin < -50 && step.Zmax < -50) || (step.Zmin > 50 && step.Zmax > 50)
    
    if xOutsideBounds || yOutsideBounds || zOutsideBounds then
        Seq.empty
    else
        seq {
            for x in step.Xmin .. step.Xmax do
                for y in step.Ymin .. step.Ymax do
                    for z in step.Zmin .. step.Zmax ->
                        (x, y, z)
            }
        |> Seq.filter (fun (x,y,z) -> x >= -50 && x <= 50 && y >= -50 && y <= 50 && z >= -50 && z <= 50)
    
let turnOn enabled step =
    step
    |> coordSeqFromStep
    |> Seq.fold (fun set element -> Set.add element set) enabled
    
let turnOff enabled step =
    step
    |> coordSeqFromStep
    |> Seq.fold (fun set element -> Set.remove element set) enabled
    
let applyInitStep enabled step =
    match step.Action with
    | On -> turnOn enabled step
    | Off -> turnOff enabled step

let initSteps = 
    getData "puzzle"
    |> Array.map parseInitStep
    
let enabledCubes =
    initSteps
    |> Seq.fold (fun enabled step -> applyInitStep enabled step) Set.empty
    
(Set.count enabledCubes).Dump()

