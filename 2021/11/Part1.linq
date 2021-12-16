<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

//let octoDump (octos: int[,]) = (octos |> Array2D. .map (fun x -> String.Join("", x))).Dump()
let octoDump (octos: int[,]) = octos.Dump()
    
let getNeighbors maxX maxY (x, y) =
    // 1 2 3
    // 4 x 6
    // 7 8 9

    let possibleNeighbors = [
        x - 1, y - 1 // 1
        x, y - 1     // 2
        x + 1, y - 1 // 3
        x - 1, y     // 4
        x + 1, y     // 6
        x - 1, y + 1 // 7
        x, y + 1     // 8
        x + 1, y + 1 // 9
    ]
    
    let isInBounds maxX maxY (x, y) =
        (x >= 0 && x <= maxX) && (y >= 0 && y <= maxY)
    
    possibleNeighbors
    |> List.filter (isInBounds maxX maxY)
    |> Set
    
let getNewFlashes maxX maxY alreadyFlashed flashesToProcess (octos: int[,]) =
    let coordinates = List.allPairs [0..maxX] [0..maxY]
    
    let flashes =
        coordinates
        |> List.filter (fun (x,y) -> octos[x,y] > 9)
        
    flashes
    |> List.except alreadyFlashed
    |> List.except flashesToProcess
    
let resetFlashedOctos octos =
    octos
    |> Array2D.map (fun o -> if o > 9 then 0 else o)

type State = {
    AlreadyFlashed: (int * int) list
    Flashes: (int * int) list
    Octos: int[,]
    TotalFlashes: int
}

let rec flash maxX maxY state =
    match state.Flashes with
    | [] -> state
    | flashPoint :: tail ->
        let flashedNeighbors = getNeighbors maxX maxY flashPoint
        
        let updatedOctos =
            state.Octos
            |> Array2D.mapi (fun x y o -> if (Set.contains (x,y) flashedNeighbors) then o + 1 else o)
                
        let newFlashes = getNewFlashes maxX maxY state.AlreadyFlashed state.Flashes updatedOctos
        
        flash maxX maxY {
            AlreadyFlashed = flashPoint :: state.AlreadyFlashed
            Flashes = List.append newFlashes tail
            Octos = updatedOctos
            TotalFlashes = state.TotalFlashes + 1
        }

let step maxX maxY (currentFlashTotal, octos:int[,]) =
    let updatedOctos =
        octos
        |> Array2D.map ((+) 1)
        
    let newFlashes = getNewFlashes maxX maxY [] [] updatedOctos
    
    let result = flash maxX maxY { AlreadyFlashed = []; Flashes = newFlashes; Octos = updatedOctos; TotalFlashes = 0 }
    
    (currentFlashTotal + result.TotalFlashes, resetFlashedOctos result.Octos)

let data = 
    getData "test"
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))
    |> array2D
    
let maxX =
    data
    |> Array2D.length1
    |> ((+) -1)

let maxY =
    data
    |> Array2D.length2
    |> ((+) -1)
    
let (total, octos) =
    seq { 1..10 }
    |> Seq.fold (fun s _ -> step maxX maxY s) (0, data)

total.Dump("total")
octoDump octos
