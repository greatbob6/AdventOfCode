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

let points =
    seq {
        for x in -50 .. 50 do
            for y in -50 .. 50 do
                for z in -50 .. 50 ->
                    (x,y,z)
        }

let totalCount =
    points
    |> PSeq.fold (fun total p -> total + ((cubeProcessor (0, p)) |> fst |> uint64)) 0UL
    
totalCount.Dump()


