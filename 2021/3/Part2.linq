<Query Kind="FSharpProgram" />

type BitCounter = {
    BitCount: int
    Count: int
}

let bitWidth = 5

let toBits (str: string) =
    Array.map (fun c -> if c = '1' then 1 else 0) (str.ToCharArray())

let codeProcessor (idx: int) (accum: BitCounter) (bitArray: int[]) =
    let bitVal = Array.item idx bitArray
    
    { accum with BitCount = accum.BitCount + bitVal; Count = accum.Count + 1 }

let countBits idx =
    Seq.fold (codeProcessor idx) { BitCount = 0; Count = 0 }
    
let mostCommonBit (totalCount: int) (oneCount: int) =
    let half = (float totalCount) / 2.
    
    if (float oneCount) < half then 0 else 1
    
let leastCommonBit totalCount oneCount =
    mostCommonBit totalCount oneCount
    |> (fun b -> (b + 1) &&& 1)

let bitsToInt = Array.rev >> (Array.mapi (fun idx bit -> bit <<< idx)) >> Array.sum

let diagnosticCodes =
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\3\puzzle_input.txt")
    |> Array.map toBits

let rec filterNumbers idx getBitVal possible =
    let accum = countBits idx possible
    let bitVal = getBitVal accum.Count accum.BitCount
    
    let filtered = possible
                    |> Array.filter (fun a -> bitVal = (Array.item idx a))
    
//    $"Idx: {idx}, BitVal: {bitVal}".Dump()
//    filtered.Dump()
    
    match filtered with
    | [| last |] -> last
    | _ -> filterNumbers (idx + 1) getBitVal filtered

let oxygenRating =
    filterNumbers 0 mostCommonBit diagnosticCodes
    |> bitsToInt

let co2Rating =
    filterNumbers 0 leastCommonBit diagnosticCodes
    |> bitsToInt
    
oxygenRating.Dump()
co2Rating.Dump()
(oxygenRating * co2Rating).Dump()