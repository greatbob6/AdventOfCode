<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

type PlayerState = {
    Position: int
    Score: int
}

type DieState = {
    LastRoll: int
    Rolls: int
}

type PlayerTurnResults = {
    PlayerState: PlayerState
    DieState: DieState
}

type GameResult = {
    PlayerOneResult: PlayerState
    PlayerTwoResult: PlayerState
    DieState: DieState
}

let rollDie (roller: DieState -> int) ds =
    let newRoll = roller ds
    
    { LastRoll = newRoll; Rolls = ds.Rolls + 1 }
    
let rollDeterministicDie = rollDie (fun ds -> (ds.LastRoll % 100) + 1)

let playerTurn dieRollFunc dieState playerState =
    let (updatedDieState, forward) =
        seq { 1 .. 3 }
        |> Seq.fold (fun (ds,sum) _ ->
            let updatedDs = dieRollFunc ds
            
            (updatedDs, sum + updatedDs.LastRoll)
        ) (dieState, 0)
        
    let newPos = ((playerState.Position + forward) % 10)
    
    {
        PlayerState = { Position = newPos; Score = playerState.Score + newPos + 1 }
        DieState = updatedDieState
    }
    
let rec playFrame dieRollFunc dieState playerOneState playerTwoState =
    let playerOneTurn = playerTurn dieRollFunc dieState playerOneState
    
    if playerOneTurn.PlayerState.Score >= 1000 then
        {
            PlayerOneResult = playerOneTurn.PlayerState
            PlayerTwoResult = playerTwoState
            DieState = playerOneTurn.DieState
        }
    else
        let playerTwoTurn = playerTurn dieRollFunc playerOneTurn.DieState playerTwoState
        
        if playerOneTurn.PlayerState.Score >= 1000 then
            {
                PlayerOneResult = playerOneTurn.PlayerState
                PlayerTwoResult = playerTwoTurn.PlayerState
                DieState = playerTwoTurn.DieState
            }
        else
            playFrame dieRollFunc playerTwoTurn.DieState playerOneTurn.PlayerState playerTwoTurn.PlayerState

let startData = 
    getData "puzzle"
    |> Array.map (fun line -> line.Split(": ")[1] |> int |> (+) -1)
    |> Array.map (fun startPos -> { Position = startPos; Score = 0 })

let gameResult =
    playFrame rollDeterministicDie { LastRoll = 0; Rolls = 0 } startData[0] startData[1]
    
let loserScore = min gameResult.PlayerOneResult.Score gameResult.PlayerTwoResult.Score
let dieRolls = gameResult.DieState.Rolls

(loserScore * dieRolls).Dump()

gameResult.Dump()
