<Query Kind="FSharpProgram" />

type BitCounter = {
    BitCount: int[]
    Count: int
}

let bitWidth = 12

let codeProcessor (accum: BitCounter) (str: string) =
    let updatedBitCount = Array.zip (str.ToCharArray()) accum.BitCount
                            |> Array.map (fun (newBit, currBit) -> currBit + (if newBit = '1' then 1 else 0))
    
    { accum with BitCount = updatedBitCount; Count = accum.Count + 1 }

let countBits = Seq.fold codeProcessor { BitCount = (Array.create bitWidth 0); Count = 0 }
    
let mostCommonBit (totalCount: int) (oneCount: int) =
    let half = totalCount / 2
    
    if oneCount < half then 0 else 1
    
let bitCounterToMostCommonBits (accum: BitCounter) = Array.map (mostCommonBit accum.Count) accum.BitCount

let bitsToInt = Array.rev >> (Array.mapi (fun idx bit -> bit <<< idx)) >> Array.sum

let gammaRate = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\3\puzzle_input.txt")
    |> countBits
    |> bitCounterToMostCommonBits
    |> bitsToInt
    
let epsilonRate = gammaRate ^^^ int (0xffffffffu >>> (32 - bitWidth))

let powerConsumption = gammaRate * epsilonRate

gammaRate.Dump()
epsilonRate.Dump()
powerConsumption.Dump()
