<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

type StackFrame = {
    OpeningChar: char
}

type Corruption = {
    OpeningChar: char
    ClosingChar: char
}

type Result =
| Correct
| Incomplete of StackFrame list
| Corruption of Corruption

let charMap = Map [
    '(', ')'
    '[', ']'
    '{', '}'
    '<', '>'
]

let isMatchingClose openChar closeChar =
    closeChar = (Map.find openChar charMap)

let rec processLine stack chars =
    match chars with
    | [] ->
        match stack with
        | [] -> Correct
        | _ -> Incomplete stack
    | char :: tail ->
        if Map.containsKey char charMap then // is opening char
            processLine ({ OpeningChar = char } :: stack) tail
        else // is closing char
            let stackFrame = List.head stack
            if isMatchingClose stackFrame.OpeningChar char then
                processLine (List.tail stack) tail
            else
                Corruption { OpeningChar = stackFrame.OpeningChar; ClosingChar = char }

let incompleteFilter = function
| Incomplete s -> Some s
| _ -> None

let getCharValue = function
| '(' -> 1UL
| '[' -> 2UL
| '{' -> 3UL
| '<' -> 4UL

let scoreLine = List.fold (fun total (stackFrame: StackFrame) -> (total * 5UL) + (getCharValue stackFrame.OpeningChar)) 0UL

let data = 
    getData "puzzle"
    |> Array.map (fun s -> s.ToCharArray() |> List.ofArray)
    |> Array.choose (fun x -> x |> (processLine []) |> incompleteFilter)
    |> Array.map scoreLine
    |> Array.sort
    |> (fun a -> a[(Array.length a) / 2])
    
data.Dump()
