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

let charMap = Map [
    '(', ')'
    '[', ']'
    '{', '}'
    '<', '>'
]

let isMatchingClose openChar closeChar =
    closeChar = (Map.find openChar charMap)

let rec findCorruption stack chars =
    //stack.Dump("stack")
    //chars.Dump("chars")
    
    match chars with
    | [] -> None
    | char :: tail ->
        if Map.containsKey char charMap then // is opening char
            findCorruption ({ OpeningChar = char } :: stack) tail
        else // is closing char
            let stackFrame = List.head stack
            if isMatchingClose stackFrame.OpeningChar char then
                findCorruption (List.tail stack) tail
            else
                (*
                stack.Dump("stack")
                chars.Dump("chars")
                $"stackFrame: {stackFrame.OpeningChar}, current char: {char}".Dump("compare closing")
                *)
                Some { OpeningChar = stackFrame.OpeningChar; ClosingChar = char }

let corruptionPoints = function
| { ClosingChar = ')' } -> 3
| { ClosingChar = ']' } -> 57
| { ClosingChar = '}' } -> 1197
| { ClosingChar = '>' } -> 25137
| _ -> 0

let data = 
    getData "puzzle"
    |> Array.map (fun s -> s.ToCharArray() |> List.ofArray)
    |> Array.choose (fun x -> findCorruption [] x)
    |> Array.map corruptionPoints
    |> Array.sum
    
data.Dump()
