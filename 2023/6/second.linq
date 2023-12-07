<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

type Race = {
    TimeLimit: int64
    Record: int64
}

let calcDistance timeLimit buttonTime = (timeLimit - buttonTime) * buttonTime

let countWins race =
    seq { 1L .. race.TimeLimit - 1L }
    |> Seq.map (calcDistance race.TimeLimit)
    |> Seq.filter (fun d -> d > race.Record)
    |> Seq.length

lines
|> Array.map (fun x -> Array.tail (Regex.Split(x, "\s+")))
|> Array.map (fun x -> String.Join("", x))
|> (fun a -> { TimeLimit = int64 a[0]; Record = int64 a[1] })
|> countWins
|> (fun x -> x.Dump())
