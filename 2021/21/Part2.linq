<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

type PlayerState = {
    Position: int
    Score: int
}

type GameResults = {
    PlayerOneWins: uint64
    PlayerTwoWins: uint64
}

type CacheKey = {
    PlayerOneState: PlayerState
    PlayerTwoState: PlayerState
    PlayerOneRolls: int
    PlayerTwoRolls: int
}

let playerTurn (dieRolls: int) playerState =
    let newPos = ((playerState.Position + dieRolls) % 10)
    
    { Position = newPos; Score = playerState.Score + newPos + 1 }

let allPossibleRollsPairs () =
    let playerOneRollPossibilities =
        seq {
            for one in 1 .. 3 do
                for two in 1 .. 3 do
                    for three in 1 .. 3 ->
                        [one; two; three]
            }
            
    let playerTwoRollPossibilities =
        seq {
            for one in 1 .. 3 do
                for two in 1 .. 3 do
                    for three in 1 .. 3 ->
                        [one; two; three]
            }
            
    Seq.allPairs playerOneRollPossibilities playerTwoRollPossibilities
    |> Seq.map (fun (p1,p2) -> (List.sum p1, List.sum p2))
    
let rec playFrame (cache: Dictionary<CacheKey, GameResults>) playerOneState playerOneRolls playerTwoState playerTwoRolls =
    let playerOneTurn = playerTurn playerOneRolls playerOneState
    
    if playerOneTurn.Score >= 21 then
        {
            PlayerOneWins = 1UL
            PlayerTwoWins = 0UL
        }
    else
        let playerTwoTurn = playerTurn playerTwoRolls playerTwoState
        
        if playerTwoTurn.Score >= 21 then
            {
                PlayerOneWins = 0UL
                PlayerTwoWins = 1UL
            }
        else
            playGame cache playerOneTurn playerTwoTurn
            
and
    playGame (cache: Dictionary<CacheKey, GameResults>) playerOneState playerTwoState =
        let allPossibleRolls = allPossibleRollsPairs ()
        
        allPossibleRolls
        |> Seq.fold (fun totalGameState (playerOneRolls, playerTwoRolls) ->
            let cacheKey = {
                PlayerOneState = playerOneState
                PlayerTwoState = playerTwoState
                PlayerOneRolls = playerOneRolls
                PlayerTwoRolls = playerTwoRolls
            }
            
            let gameResult =
                if cache.ContainsKey(cacheKey) then
                    cache[cacheKey]
                else
                    let result = playFrame cache playerOneState playerOneRolls playerTwoState playerTwoRolls
                    
                    cache.Add(cacheKey, result)
                    
                    result
            
            {
                PlayerOneWins = totalGameState.PlayerOneWins + gameResult.PlayerOneWins
                PlayerTwoWins = totalGameState.PlayerTwoWins + gameResult.PlayerTwoWins
            }
        ) { PlayerOneWins = 0UL; PlayerTwoWins = 0UL }

let startData = 
    getData "puzzle"
    |> Array.map (fun line -> line.Split(": ")[1] |> int |> (+) -1)
    |> Array.map (fun startPos -> { Position = startPos; Score = 0 })

let cache = new Dictionary<CacheKey, GameResults>()

let gameResult =
    playGame cache startData[0] startData[1]
    
gameResult.Dump()

//630947104784464: I have a logic error so player 1's result is actually 27 times too big
//485695191963177
