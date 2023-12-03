<Query Kind="FSharpProgram" />

let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")


let test = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

type Draw = {
    Red: int
    Green: int
    Blue: int
}

type Game = {
    GameId: int
    Draws: Draw list
}

let parseGame (gameString: string) =
    let parseGameId (gameStart: string) = gameStart.Substring(5, gameStart.Length - 5) |> int
    let parseDraw (elements: string array) =
        let elementsSplit = elements |> Array.map (fun x -> x.Split(" "))
        
        Array.fold (fun d (e: string array) ->
            let num = int e[0]
            
            match e[1] with
            | "red" -> { d with Red = num }
            | "green" -> { d with Green = num }
            | "blue" -> { d with Blue = num }
            | _ -> d
        ) { Red = 0; Green = 0; Blue = 0 } elementsSplit
        
    let parseDraws (drawParts: string array) =
        drawParts
        |> Array.map (fun s -> s.Split(",") |> Array.map (fun x -> x.Trim()))
        |> Array.map parseDraw
        |> Array.toList
    
    let parts = gameString.Split(":")
    let drawParts = parts[1].Split(";")
    
    let gameId = parseGameId parts[0]
    let draws = parseDraws drawParts
    
    { GameId = gameId; Draws = draws }
    
let gameTester g =
    let maxRed = 12
    let maxGreen = 13
    let maxBlue = 14
    
    g.Draws
    |> List.forall (fun d -> d.Red <= maxRed && d.Green <= maxGreen && d.Blue <= maxBlue)

lines
|> Array.map parseGame
|> Array.filter gameTester
|> Array.map (fun g -> g.GameId)
|> Array.sum
|> (fun x -> x.Dump())
