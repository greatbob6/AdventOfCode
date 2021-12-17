<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

type Node = {
    Name: string
    IsLargeCave: bool
    Connections: string list
}

let makeNode name conns =
    { Name = name; IsLargeCave = (name = name.ToUpper()); Connections = List.ofArray conns }

let rec explore nodeMap foundPaths currentPath nodeName =
    let node = Map.find nodeName nodeMap
    
    let visited = List.contains node currentPath
    let smallCanVisitTwice =
        currentPath
        |> List.filter (fun n -> not n.IsLargeCave)
        |> List.countBy (fun n -> n.Name)
        |> List.forall (fun (_, count) -> count <= 1)

    // Part 1 conditional
    //let canVisit = (not visited) || (node.IsLargeCave)
    
    // Part 2 conditional
    let canVisit = (not visited) || (node.IsLargeCave || (smallCanVisitTwice && node.Name <> "start"))
    
    if node.Name = "end" then
        (node :: currentPath) :: foundPaths
    elif not canVisit then
        foundPaths
    else
        node.Connections
        |> List.collect (explore nodeMap foundPaths (node :: currentPath))

let connectionData = 
    getData "puzzle"
    |> Array.map (fun s ->
        let splitStr = s.Split("-")
        (splitStr[1], splitStr[0])
    )

let nodes =
    connectionData
    |> Array.map (fun (a,b) -> (b,a))
    |> Array.append connectionData
    |> Array.groupBy (fun (a,b) -> a)
    |> Array.map (fun (name, connections) -> (name, makeNode name (Array.map snd connections)))
    |> Map

let paths = explore nodes [] [] "start"

let pathDump paths =
    let cleanupPath p =
        p
        |> List.map (fun n -> n.Name)
        |> List.rev
        |> (fun l -> String.Join(",", l))
        
    (paths
    |> List.map cleanupPath
    |> List.sort).Dump()
    
pathDump paths