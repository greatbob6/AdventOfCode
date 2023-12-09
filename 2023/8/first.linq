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

let start = Map.find "AAA" nodeMap
let last = Map.find "ZZZ" nodeMap

let rec nodeDiver node instructionIdx actionCount =
    //$"{node.Label} - {instructions[instructionIdx]} - {actionCount}".Dump()
    
    if node.Label = last.Label then
        actionCount
    else
        let nextNodeLabel = if instructions[instructionIdx] = 'L' then node.Left else node.Right
        let nextNode = Map.find nextNodeLabel nodeMap
        
        nodeDiver nextNode ((instructionIdx + 1) % instructionsLength) (actionCount + 1)

nodeDiver start 0 0
|> dumper
