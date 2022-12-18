
type Point = {
    X: int
    Y: int
}

type State = {
    Points: Point list
    TailPointsVisited: Set<Point>
}

type Move =
| Up of int
| Down of int
| Left of int
| Right of int

let parseMoves lines =
    let parseLine str =
        let m = System.Text.RegularExpressions.Regex.Match(str, "([UDLR]) ([0-9]+)")

        match m.Groups[1].Value with
        | "U" -> Up (int m.Groups[2].Value)
        | "D" -> Down (int m.Groups[2].Value)
        | "L" -> Left (int m.Groups[2].Value)
        | "R" -> Right (int m.Groups[2].Value)
        | _ -> failwith "Bad data"
    
    lines |> Array.map parseLine

let diff point1 point2 =
    let x = point1.X - point2.X
    let y = point1.Y - point2.Y

    (x, y)

let moveFollower leader follower =
    let d = diff leader follower

    match d with
    | (2, 0) -> { follower with X = follower.X + 1 }
    | (-2, 0) -> { follower with X = follower.X - 1 }
    | (0, 2) -> { follower with Y = follower.Y + 1 }
    | (0, -2) -> { follower with Y = follower.Y - 1 }
    | (1, 2) -> { follower with X = follower.X + 1; Y = follower.Y + 1 }
    | (-1, 2) -> { follower with X = follower.X - 1; Y = follower.Y + 1 }
    | (-1, -2) -> { follower with X = follower.X - 1; Y = follower.Y - 1 }
    | (1, -2) -> { follower with X = follower.X + 1; Y = follower.Y - 1 }
    | (2, -1) -> { follower with X = follower.X + 1; Y = follower.Y - 1 }
    | (2, 1) -> { follower with X = follower.X + 1; Y = follower.Y + 1 }
    | (-2, 1) -> { follower with X = follower.X - 1; Y = follower.Y + 1 }
    | (-2, -1) -> { follower with X = follower.X - 1; Y = follower.Y - 1 }
    | (2, -2) -> { follower with X = follower.X + 1; Y = follower.Y - 1 }
    | (2, 2) -> { follower with X = follower.X + 1; Y = follower.Y + 1 }
    | (-2, 2) -> { follower with X = follower.X - 1; Y = follower.Y + 1 }
    | (-2, -2) -> { follower with X = follower.X - 1; Y = follower.Y - 1 }
    | _ -> follower

let mover (state: State) count (headUpdater: (Point -> Point)) =
    seq { 0 .. count - 1 }
    |> Seq.fold (fun s v ->
        let newHead = headUpdater s.Points[0]

        let rec moveElement leader followerIdx (originalList: Point list) updatedList =
            if followerIdx > 9 then
                List.rev updatedList
            else
                let newVal = moveFollower leader originalList[followerIdx]

                moveElement newVal (followerIdx + 1) originalList (newVal :: updatedList)

        let updatedPoints = moveElement newHead 1 s.Points [ newHead ]

        { s with Points = updatedPoints; TailPointsVisited = Set.add updatedPoints[9] s.TailPointsVisited }
    ) state

let moveUp state count =
    mover state count (fun h -> { h with Y = h.Y + 1 })

let moveDown state count =
    mover state count (fun h -> { h with Y = h.Y - 1 })

let moveLeft state count =
    mover state count (fun h -> { h with X = h.X - 1 })

let moveRight state count =
    mover state count (fun h -> { h with X = h.X + 1 })

let applyMove state move =
    match move with
    | Up x -> moveUp state x
    | Down x -> moveDown state x
    | Left x -> moveLeft state x
    | Right x -> moveRight state x

let start = { Points = [ for i in 0 .. 9 -> { X = 0; Y = 0 } ]; TailPointsVisited = Set.empty<Point> }


let result =
    System.IO.File.ReadAllLines(@"9/puzzle.txt")
    |> parseMoves
    |> Array.fold (fun s m -> applyMove s m ) start


//let result = applyMove start (Right 5)
//let result2 = applyMove result (Up 8)

//printState result

Set.count result.TailPointsVisited