// Rock = 1
// Paper = 2
// Scissors = 3

type Selection =
    | Rock
    | Paper
    | Scissors

type DesiredResult =
    | Lose
    | Draw
    | Win

type GameRound = {
    Opponent: Selection
    Mine: Selection
}

let parseSelection = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    | x -> failwith $"Error: {x}"

let parseDesiredResult = function
    | 'X' -> Lose
    | 'Y' -> Draw
    | 'Z' -> Win
    | x -> failwith $"Error: {x}"

let calculateMySelection opponent desiredRes =
    match desiredRes with
    | Draw -> opponent
    | Win -> match opponent with
             | Rock -> Paper
             | Paper -> Scissors
             | Scissors -> Rock
    | Lose -> match opponent with
              | Rock -> Scissors
              | Paper -> Rock
              | Scissors -> Paper

let parseGameRound (str: string) =
    let opponent = parseSelection str[0]
    let desiredRes = parseDesiredResult str[2]

    {
        Opponent = opponent
        Mine = (calculateMySelection opponent desiredRes)
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
