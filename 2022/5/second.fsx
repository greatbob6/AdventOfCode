let splitIntoStartAndInstructions lines =
    let idx = Array.findIndex (fun x -> x = "") lines
    let start = lines[..idx - 1]
    let instructions = lines[idx + 1..]

    start, instructions

let processStart (lines: string array) =
    lines
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.transpose
    |> Array.map Array.rev
    |> Array.filter (fun c -> System.Char.IsDigit(c[0]))
    |> Array.map (fun x -> Array.filter (fun c -> c <> ' ') x)
    |> Array.sortBy (fun x -> x[0])
    |> Array.map (fun x -> x[1..])
    |> Array.map (Array.rev >> List.ofArray)
    |> List.ofArray

type Instruction = {
    Count: int
    Source: int
    Destination: int
}

let parseInstructions lines =
    let parseInstr s =
        let matches = System.Text.RegularExpressions.Regex.Match(s, "move ([0-9]+) from ([0-9]+) to ([0-9]+)")
        { Count = int(matches.Groups[1].Value); Source = int(matches.Groups[2].Value); Destination = int(matches.Groups[3].Value) }

    lines
    |> Array.map parseInstr

let applyInstruction (stacks: char list list) instruction =
    let sourceStack = stacks[instruction.Source - 1]
    let destinationStack = stacks[instruction.Destination - 1]

    //let newDest = List.fold (fun s t -> t :: s) destinationStack sourceStack[..instruction.Count - 1]
    let newDest = List.append sourceStack[..instruction.Count - 1] destinationStack
    let newSrc = sourceStack[instruction.Count..]

    stacks
    |> List.removeAt (instruction.Source - 1)
    |> List.insertAt (instruction.Source - 1) newSrc
    |> List.removeAt (instruction.Destination - 1)
    |> List.insertAt (instruction.Destination - 1) newDest

let (startLines, instructionLines) =
    System.IO.File.ReadAllLines("5/puzzle.txt")
    |> splitIntoStartAndInstructions

let startStacks = processStart startLines
let instructions = parseInstructions instructionLines

instructions
|> Array.fold applyInstruction startStacks
|> List.map List.head
|> Array.ofList
|> System.String