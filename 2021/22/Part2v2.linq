<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Collections.ParallelSeq</NuGetReference>
</Query>

open FSharp.Collections.ParallelSeq

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
    
type WorkingBoundary =
    {
        minX: int
        maxX: int
        minY: int
        maxY: int
        minZ: int
        maxZ: int
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
    
let initStepToFunc step =
    match step.Action with
    | On -> (fun (curVal, (x,y,z)) ->
            if x >= step.Xmin && x <= step.Xmax && y >= step.Ymin && y <= step.Ymax && z >= step.Zmin && z <= step.Zmax then
                (1, (x,y,z))
            else
                (curVal, (x,y,z))
        )
    | Off -> (fun (curVal, (x,y,z)) ->
            if x >= step.Xmin && x <= step.Xmax && y >= step.Ymin && y <= step.Ymax && z >= step.Zmin && z <= step.Zmax then
                (0, (x,y,z))
            else
                (curVal, (x,y,z))
        )

let initSteps = 
    getData "puzzle"
    |> Array.map parseInitStep

let cubeProcessor =
    initSteps
    |> Array.map initStepToFunc
    |> Array.fold (fun composition func -> composition >> func) id

let outerDims =
    let ons = Array.filter (fun s -> s.Action = On) initSteps
    
    {
        minX = ons |> Array.map (fun s -> s.Xmin) |> Array.min
        maxX = ons |> Array.map (fun s -> s.Xmax) |> Array.max
        minY = ons |> Array.map (fun s -> s.Ymin) |> Array.min
        maxY = ons |> Array.map (fun s -> s.Ymax) |> Array.max
        minZ = ons |> Array.map (fun s -> s.Zmin) |> Array.min
        maxZ = ons |> Array.map (fun s -> s.Zmax) |> Array.max
    }

outerDims.Dump()

let points =
    seq {
        for x in outerDims.minX .. outerDims.maxX do
            for y in outerDims.minY .. outerDims.maxY do
                for z in outerDims.minZ .. outerDims.maxZ ->
                    (x,y,z)
        }

let totalCount =
    points
    |> PSeq.fold (fun total p -> total + ((cubeProcessor (0, p)) |> fst |> uint64)) 0UL
    
totalCount.Dump()

