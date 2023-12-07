<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

type Race = {
    TimeLimit: int
    Record: int
}

let calcDistance timeLimit buttonTime = (timeLimit - buttonTime) * buttonTime

let countWins race =
    seq { 1 .. race.TimeLimit - 1 }
    |> Seq.map (calcDistance race.TimeLimit)
    |> Seq.filter (fun d -> d > race.Record)
    |> Seq.length

lines
|> Array.map (fun x -> Array.tail (Regex.Split(x, "\s+")))
|> (fun a -> Array.zip a[0] a[1] |> Array.map (fun (x,y) -> { TimeLimit = int x; Record = int y }))
|> Array.map countWins
|> Array.reduce (*)
|> (fun x -> x.Dump())
