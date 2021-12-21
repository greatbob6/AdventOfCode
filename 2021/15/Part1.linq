<Query Kind="FSharpExpression" />

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

let getNeighbors (x, y) =
    let isInBounds (px, py) = (px >= 0 && px <= maxX) && (py >= 0 && py <= maxY)
    
    [| (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y) |]
    |> Array.filter isInBounds

let getGScore gScore current =
    if Map.containsKey current gScore then
        Map.find current gScore
    else
        Int32.MaxValue
        
let distanceToDestination (cx, cy) = (maxX - cx) + (maxY - cy)
let d (nx, ny) = positionData[ny, nx]

let rec reconstruct_path (cameFrom: Map<int*int, int*int>) (totalPath: (int*int) list) (current: int*int) =
    match Map.tryFind current cameFrom with
    | Some prev -> reconstruct_path cameFrom (current :: totalPath) prev
    | None -> (current :: totalPath)

// A* finds a path from start to goal.
// h is the heuristic function. h(n) estimates the cost to reach goal from node n.
let A_Star (start: int*int) (goal: int*int) (h: (int*int)->int) =
    let rec step state =
        let (gScores, fScores, (openSet: Set<int*(int*int)>), cameFrom) = state
        
        if Set.isEmpty openSet then
            state
        else
            let (currentFScore, current) = Set.minElement openSet
            
            if current = goal then
                state
            else
                let updatedOpenSet = Set.remove (currentFScore, current) openSet
                let neighbors = getNeighbors current
                
                let newState =
                    neighbors
                    |> Array.fold (fun (gs, fs, os, cf) neighbor ->
                        let tentative_gScore = (getGScore gs current) + (d neighbor)
                        if tentative_gScore < (getGScore gs neighbor) then
                            // This path to neighbor is better than any previous one. Record it!
                            let updatedCf = Map.change neighbor (fun x -> Some current) cf
                            let updatedGs = Map.change neighbor (fun x -> Some tentative_gScore) gs
                            let updatedFs = Map.change neighbor (fun x -> Some (tentative_gScore + (h neighbor))) fs
                            
                            let updatedOs = os//if Set.contains neighbor os then os else (Set.add ((Map.find neighbor updatedFs), neighbor) os)
                            
                            (updatedGs, updatedFs, updatedOs, updatedCf)
                        else
                            (gs, fs, os, cf)
                        ) (gScores, fScores, updatedOpenSet, cameFrom)
                        
                step newState
        
    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    // This is usually implemented as a min-heap or priority queue rather than a hash-set.
    let openSet = Set [ 0,start ]

    // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from start
    // to n currently known.
    let cameFrom = Map.empty<int*int, int*int>

    // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
    //gScore := map with default value of Infinity
    //gScore[start] := 0
    let gScore = Map [ (start, 0) ]

    // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
    // how short a path from start to finish can be if it goes through n.
    //fScore := map with default value of Infinity
    //fScore[start] := h(start)
    let fScore = Map [ (start, h(start)) ]

    let (gs, fs, os, cf) = step (gScore, fScore, openSet, cameFrom)
    
    reconstruct_path cf [] destination
    
    
let result = A_Star (0,0) destination distanceToDestination

result.Dump()
