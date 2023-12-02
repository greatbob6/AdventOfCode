<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

lines
|> Array.map (fun s -> s.ToCharArray())
|> Array.map (fun ca -> [| Array.find Char.IsDigit ca; Array.findBack Char.IsDigit ca |])
|> Array.map (fun ca -> String ca |> int)
|> Array.sum
|> (fun x -> x.Dump())