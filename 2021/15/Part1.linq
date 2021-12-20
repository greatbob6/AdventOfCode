<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

let positionData = 
    getData "puzzle"
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))
    |> array2D

let maxX = (Array2D.length2 positionData) - 1
let maxY = (Array2D.length1 positionData) - 1

let destination = (maxX, maxY)
    
type Path = {
    Cost: int
    Positions: (int * int) list
}

let getNeighbors (x, y) =
    let isInBounds (px, py) = (px >= 0 && px <= maxX) && (py >= 0 && py <= maxY)
    
    [| (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y) |]
    |> Array.filter isInBounds

let rec explore (bestPath: Path) (currentPath: Path) location =
    let visited = List.contains location currentPath.Positions
    
    if visited then
        bestPath
    else
        let updatedPath = { Cost = currentPath.Cost + positionData[(snd location), (fst location)]; Positions = location :: currentPath.Positions }
        
        if updatedPath.Cost >= bestPath.Cost then
            bestPath
        elif location = destination then
            updatedPath.Cost.Dump("Found destination")
            updatedPath
        else
            getNeighbors location
            |> Array.sortBy (fun (nx, ny) -> positionData[ny, nx])
            |> Array.fold (fun bestSoFar loc -> explore bestSoFar updatedPath loc) bestPath
            |> (fun path -> if path.Cost < bestPath.Cost then path else bestPath)
            
destination.Dump("destination")

let rec findFirstPath currentPath (x,y) =
    let updatedPath = { Cost = currentPath.Cost + positionData[y, x]; Positions = (x,y) :: currentPath.Positions }
    
    if (x,y) = destination then
        updatedPath
    else
        let nextLoc =
            if x = maxX then
                (x, y + 1)
            else
                (x + 1, y)
                
        findFirstPath updatedPath nextLoc
    
let firstPath = findFirstPath { Cost = 0; Positions = [] } (0, 0)

(firstPath.Cost - positionData[0, 0]).Dump("First Path Cost")

let result = explore firstPath { Cost = 0; Positions = [] } (0, 0)

(result.Cost - positionData[0, 0]).Dump("Best cost")
