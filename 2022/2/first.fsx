// Rock = 1
// Paper = 2
// Scissors = 3

type Selection =
    | Rock
    | Paper
    | Scissors

let parseSelection = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    | x -> failwith $"Error: {x}"

type GameRound = {
    Opponent: Selection
    Mine: Selection
}

let parseGameRound (str: string) =
    {
        Opponent = parseSelection str[0]
        Mine = parseSelection str[2]
    }

let calculateSelectionScore = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let isMyWin = function
    | { Opponent = Rock; Mine = Paper } -> true
    | { Opponent = Paper; Mine = Scissors } -> true
    | { Opponent = Scissors; Mine = Rock } -> true
    | _ -> false    

let calculateRoundResultScore rnd =
    if rnd.Opponent = rnd.Mine then 3
    elif isMyWin rnd then 6
    else 0 

let calculateScoreForRound rnd =
    calculateRoundResultScore rnd + calculateSelectionScore rnd.Mine

let lines = System.IO.File.ReadAllLines("2/puzzle.txt")

let finalScore =
    lines
    |> Array.map parseGameRound
    |> Array.map calculateScoreForRound
    |> Array.sum
