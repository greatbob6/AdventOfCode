<Query Kind="FSharpProgram" />

let dumper x = x.Dump()
let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

let nodeRegex = new Regex("(\w{3}) = \((\w{3}), (\w{3})\)", RegexOptions.Compiled)

type Node = {
    Label: string
    Left: string
    Right: string
}

let parseNode s =
    let m = nodeRegex.Match(s)
    
    { Label = m.Groups[1].Value; Left = m.Groups[2].Value; Right = m.Groups[3].Value }

let instructions = lines[0].ToCharArray()
let instructionsLength = Array.length instructions

let nodes =
    lines[2..]
    |> Array.map parseNode
    
let nodeMap = nodes |> Array.map (fun n -> (n.Label, n)) |> Map.ofArray

let startingNodes = nodes |> Array.filter (fun n -> n.Label.EndsWith("A"))

let completed na = na |> Array.forall (fun n -> n.Label.EndsWith("Z"))

let rec nodeDiver currentNodes instructionIdx actionCount =
    if completed currentNodes then
        actionCount
    else
        let nextNodes =
            currentNodes
            |> Array.map (fun n ->
                let nextNodeLabel = if instructions[instructionIdx] = 'L' then n.Left else n.Right
                
                Map.find nextNodeLabel nodeMap
            )
        nodeDiver nextNodes ((instructionIdx + 1) % instructionsLength) (actionCount + 1)

nodeDiver startingNodes 0 0
|> dumper
