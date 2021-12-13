<Query Kind="FSharpProgram" />

type Test = {
    Inputs: char[][]
    OutputDigits: char[][]
}

let parseInsAndOuts [| (is: string); (os: string) |] =
    let inputs =
        is.Split(" ")
        |> Array.map (fun x -> x.ToCharArray())
        
    let outputs =
        os.Split(" ")
        |> Array.map (fun x -> x.ToCharArray())
    
    { Inputs = inputs; OutputDigits = outputs }

let lengthMatches len = Array.length >> ((=) len)

let doesNotContain exclude x =
    let excepted = Array.except exclude x
    
    x.Dump()
    excepted.Dump("excepted")
    
    Array.length x = Array.length excepted
    
(*
let findThree digitMap inputs =
    let oneChars = Map.find 1 digitMap
    let eightChars = Map.find 8 digitMap
    
    let threeChars =
        Array.except oneChars eightChars
        |> Array.sort
    
    threeChars.Dump("threeChars")
    
    let pred x =
        let sortedX = Array.sort x
        
        sortedX.Dump("sortedX")
        
        let res = (lengthMatches 5 x) && (sortedX = threeChars)
        res.Dump()
        res
    
    Array.find pred inputs
*)

let data = 
    System.IO.File.ReadAllLines(@"C:\Users\great\OneDrive\Documents\Programming\AdventOfCode\2021\8\test_input.txt")
    |> Array.map (fun s -> s.Split(" | "))
    |> Array.map parseInsAndOuts

let firstLine = Array.head data

let digitMap' = Map [
    1, Array.find (lengthMatches 2) firstLine.Inputs
    4, Array.find (lengthMatches 4) firstLine.Inputs
    7, Array.find (lengthMatches 3) firstLine.Inputs
    8, Array.find (lengthMatches 7) firstLine.Inputs
]

let six = findSix digitMap' firstLine.Inputs
six.Dump()

//let digitMap = Map.add 3 (findThree digitMap' firstLine.Inputs) digitMap'
//digitMap.Dump()
