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

type WorkingBoundaries =
    {
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

let coordSeqFromStep boundaries (step: InitStep) =
    let xOutsideBounds = (step.Xmin < boundaries.Xmin && step.Xmax < boundaries.Xmin) || (step.Xmin > boundaries.Xmax && step.Xmax > boundaries.Xmax)
    let yOutsideBounds = (step.Ymin < boundaries.Ymin && step.Ymax < boundaries.Ymin) || (step.Ymin > boundaries.Ymax && step.Ymax > boundaries.Ymax)
    let zOutsideBounds = (step.Zmin < boundaries.Zmin && step.Zmax < boundaries.Zmin) || (step.Zmin > boundaries.Zmax && step.Zmax > boundaries.Zmax)
    
    if xOutsideBounds || yOutsideBounds || zOutsideBounds then
        Seq.empty
    else
        seq {
            for x in step.Xmin .. step.Xmax do
                for y in step.Ymin .. step.Ymax do
                    for z in step.Zmin .. step.Zmax ->
                        (x, y, z)
            }
        |> Seq.filter (fun (x,y,z) -> x >= boundaries.Xmin && x < boundaries.Xmax && y >= boundaries.Ymin && y < boundaries.Ymax && z >= boundaries.Zmin && z < boundaries.Zmax)
    
let turnOn boundaries enabled (step: InitStep) =
    step
    |> (coordSeqFromStep boundaries)
    |> Seq.fold (fun set element -> Set.add element set) enabled
    
let turnOff boundaries enabled (step: InitStep) =
    step
    |> (coordSeqFromStep boundaries)
    |> Seq.fold (fun set element -> Set.remove element set) enabled
    
let applyInitStep boundaries enabled step =
    match step.Action with
    | On -> turnOn boundaries enabled step
    | Off -> turnOff boundaries enabled step
    
let countCubes region steps =
    steps
    |> Seq.fold (fun enabled step -> applyInitStep region enabled step) Set.empty
    |> Seq.length
    |> uint64

let initSteps = 
    getData "test_part2"
    |> Array.map parseInitStep

let outerDims =
    let ons = Array.filter (fun s -> s.Action = On) initSteps
    
    let dim =
        ons
        |> Array.collect (fun s -> [|abs s.Xmin; abs s.Xmax; abs s.Ymin; abs s.Ymax; abs s.Zmin; abs s.Zmax|])
        |> Array.max
        |> (fun d -> int ((ceil (float d / 2000.)) * 2000.))
        
    {
        Xmin = -dim
        Xmax = dim
        Ymin = -dim
        Ymax = dim
        Zmin = -dim
        Zmax = dim
    }

outerDims.Dump()

let blocks =
    seq {
        for x in outerDims.Xmin .. 2000 .. outerDims.Xmax do
            for y in outerDims.Ymin .. 2000 .. outerDims.Ymax do
                for z in outerDims.Zmin .. 2000 .. outerDims.Zmax ->
                    {
                        Xmin = x
                        Xmax = x + 2000
                        Ymin = y
                        Ymax = y + 2000
                        Zmin = z
                        Zmax = z + 2000
                    }
        }
    |> Seq.toArray
    
(Array.length blocks).Dump("Blocks")

let totalCount =
    blocks
    |> Array.Parallel.map (fun b -> countCubes b initSteps)
    |> Array.sum

(totalCount).Dump()

