<Query Kind="FSharpProgram" />

type Test = {
    Inputs: char[][]
    OutputDigits: char[][]
}

let parseInsAndOuts [| (is: string); (os: string) |] =
    let inputs =
        is.Split(" ")
        |> Array.map (fun x -> Array.sort (x.ToCharArray()))
        
    let outputs =
        os.Split(" ")
        |> Array.map (fun x -> Array.sort (x.ToCharArray()))
    
    { Inputs = inputs; OutputDigits = outputs }

let lengthMatches len = Array.length >> ((=) len)

let findSix inputs (digitMap, segmentMap) =
    let eightChars = Map.find 8 digitMap
    let oneChars = Map.find 1 digitMap
    
    let makeSix eightChars char =
        Array.except [ char ] eightChars
    
    let findInInputs chars inputs =
        inputs
        |> Array.tryFind (fun input -> input = chars)
    
    let cAndSix =
        oneChars
        |> Array.map (fun char -> char, findInInputs (makeSix eightChars char) inputs)
        |> Array.filter (fun (_, six) -> Option.isSome six)
        |> Array.head
    
    let cSegment = fst cAndSix
    let fSegment = Array.except [ cSegment ] oneChars |> Array.head
        
    let six = Option.get (snd cAndSix)

    let updatedSegmentMap =
        segmentMap
        |> Map.add 'f' fSegment
        |> Map.add 'c' cSegment
        
    let updatedDigitMap =
        digitMap
        |> Map.add 6 six
        
    (updatedDigitMap, updatedSegmentMap)
    
let findTwo inputs (digitMap, segmentMap) =
    let fChar = Map.find 'f' segmentMap
    
    let two = 
        inputs
        |> Array.find (fun i -> (Array.contains fChar i) |> not)
    
    (digitMap |> Map.add 2 two, segmentMap)

let findFive inputs (digitMap, segmentMap) =
    let cChar = Map.find 'c' segmentMap
    let sixChars = Map.find 6 digitMap
    
    let five =
        inputs
        |> Array.find (fun i -> i <> sixChars && (Array.contains cChar i) |> not)
        
    let eChar =
        Array.except five sixChars
        |> Array.head
        
    (digitMap |> Map.add 5 five, segmentMap |> Map.add 'e' eChar)
    
let findNine _ (digitMap, segmentMap) =
    let eChar = Map.find 'e' segmentMap
    let eightChars = Map.find 8 digitMap
    
    let nine = Array.except [eChar] eightChars
    
    (digitMap |> Map.add 9 nine, segmentMap)

let findG (digitMap, segmentMap) =
    let aChar = Map.find 'a' segmentMap
    let nineChars = Map.find 9 digitMap
    let fourChars = Map.find 4 digitMap
    
    let gChar =
        nineChars
        |> Array.except fourChars
        |> Array.except [aChar]
        |> Array.head
        
    (digitMap, segmentMap |> Map.add 'g' gChar)
    
let findB (digitMap, segmentMap) =
    let fChar = Map.find 'f' segmentMap
    let eightChars = Map.find 8 digitMap
    let twoChars = Map.find 2 digitMap
    
    let bChar =
        eightChars
        |> Array.except twoChars
        |> Array.except [fChar]
        |> Array.head
        
    (digitMap, segmentMap |> Map.add 'b' bChar)
    
let findD (digitMap, segmentMap) =
    let dChar =
        [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' |]
        |> Array.except (Map.values segmentMap)
        |> Array.head

    (digitMap, segmentMap |> Map.add 'd' dChar)

let buildThree (digitMap, segmentMap) =
    let three =
        [|
            Map.find 'a' segmentMap
            Map.find 'c' segmentMap
            Map.find 'd' segmentMap
            Map.find 'f' segmentMap
            Map.find 'g' segmentMap
        |]
        |> Array.sort
        
    (digitMap |> Map.add 3 three, segmentMap)

let buildZero (digitMap, segmentMap) =
    let zero =
        [|
            Map.find 'a' segmentMap
            Map.find 'b' segmentMap
            Map.find 'c' segmentMap
            Map.find 'e' segmentMap
            Map.find 'f' segmentMap
            Map.find 'g' segmentMap
        |]
        |> Array.sort
        
    (digitMap |> Map.add 0 zero, segmentMap)
    
let processLine line =
    let initialDigitMap = Map [
        1, Array.find (lengthMatches 2) line.Inputs
        4, Array.find (lengthMatches 4) line.Inputs
        7, Array.find (lengthMatches 3) line.Inputs
        8, Array.find (lengthMatches 7) line.Inputs
    ]

    let initialSegmentMap = Map [
        'a', Array.except (Map.find 1 initialDigitMap) (Map.find 7 initialDigitMap) |> Array.head
    ]

    let (digitMap, segmentMap) =
        (initialDigitMap, initialSegmentMap)
        |> findSix line.Inputs
        |> findTwo line.Inputs
        |> findFive line.Inputs
        |> findNine line.Inputs
        |> findG
        |> findB
        |> findD
        |> buildThree
        |> buildZero

    let outputNum =
        line.OutputDigits
        |> Array.map (fun chars -> Map.findKey (fun _ digitChars -> chars = digitChars) digitMap)
        |> Array.map string
        |> String.concat ""
        |> int

    outputNum

let data = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\8\puzzle_input.txt")
    |> Array.map (fun s -> s.Split(" | "))
    |> Array.map parseInsAndOuts

let firstLine = Array.head data

let result =
    data
    |> Array.map processLine
    |> Array.sum

result.Dump()
