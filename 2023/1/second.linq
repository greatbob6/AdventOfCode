<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let mapDigitNames x =
    match x with
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9

let digitNames = [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let line = "eightwothree"

let findAll (d: string) (s: string) =
    let rec finder (startIdx: int) acc =
        let idx = s.IndexOf(d, startIdx)
        
        if idx < 0 then
            acc
        else
            finder (idx + 1) ((mapDigitNames d, idx) :: acc)
            
    finder 0 []

let findAllDigitNames (s: string) =
    digitNames
    |> List.collect (fun d -> findAll d s)
    
let findAllDigits (s: string) =
    s.ToCharArray()
    |> Array.mapi (fun idx c -> if Char.IsDigit c then Some (int (string c), idx) else None)
    |> Array.choose id
    |> Array.toList

let getNumberFromDigits s =
    let allDigitNames = findAllDigitNames s
    let allDigits = findAllDigits s

    List.concat [ allDigitNames; allDigits ]
    |> List.sortBy (fun (a,b) -> b)
    |> List.map (fun (a,b) -> a)
    |> (fun l -> (List.head l) * 10 + (List.last l))

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

lines
|> Array.map getNumberFromDigits
|> Array.sum
|> (fun x -> x.Dump())
