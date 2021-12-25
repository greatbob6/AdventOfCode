<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

type Target = {
    minX: int
    maxX: int
    minY: int
    maxY: int
}

type TrajectoryLaunch = {
    Positions: (int * int) list
    HitTarget: bool
    MissedTarget: bool
}

let targetParser = new Regex("target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")
    
let target = 
    getData "puzzle"
    |> Array.head
    |> (fun l -> targetParser.Match(l))
    |> (fun m -> {
            minX = (int m.Groups[1].Value)
            maxX = (int m.Groups[2].Value)
            minY = (int m.Groups[3].Value)
            maxY = (int m.Groups[4].Value)
        })

let posX vx0 n =
    let vxn vx0 i = max (vx0 - (i - 1)) 0
    
    [1..n]
    |> List. fold (fun total i -> total + (vxn vx0 i)) 0
    
let posY vy0 n = n * vy0 - ((n * (n - 1)) / 2)

let isInTarget x y =
    (x >= target.minX && x <= target.maxX) && (y >= target.minY && y <= target.maxY)
    
let isPastTarget _ y =
    (y < target.minY)

let rec generatePositionList positions i vx0 vy0 =
    let px = posX vx0 i
    let py = posY vy0 i
    
    let inTarget = isInTarget px py
    let pastTarget = isPastTarget px py
    
    if inTarget then
        Some ((px,py) :: positions)
    elif pastTarget then
        None
    else
        generatePositionList ((px,py) :: positions) (i + 1) vx0 vy0

target.Dump()

let vx0range = seq { 1 .. target.maxX }
let vy0range = seq { 1 .. 100 }

let result = 
    Seq.allPairs vx0range vy0range
    |> Seq.choose (fun (vx0, vy0) -> generatePositionList [] 1 vx0 vy0)
    |> Seq.map (fun l -> List.maxBy (fun (_,y) -> y) l |> snd)
    |> Seq.max
    
result.Dump()
