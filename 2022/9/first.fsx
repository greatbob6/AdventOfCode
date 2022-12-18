
type Point = {
    X: int
    Y: int
}

type State = {
    Head: Point
    Tail: Point
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

let printState state =
    let minX = min state.Head.X state.Tail.X
    let maxX = max state.Head.X state.Tail.X
    let minY = min state.Head.Y state.Tail.Y
    let maxY = max state.Head.Y state.Tail.Y

    (seq { maxY .. -1 .. minY })
    |> Seq.iter (fun y ->
        (seq { minX .. maxX })
        |> Seq.iter (fun x ->
            let o =
                if state.Head.X = x && state.Head.Y = y && state.Tail.X = x && state.Tail.Y = y then "B"
                elif state.Head.X = x && state.Head.Y = y then "H"
                elif state.Tail.X = x && state.Tail.Y = y then "T"
                else "-"

            System.Console.Write(o)
            )

        System.Console.WriteLine()
    )

let diff head tail =
    let x = head.X - tail.X
    let y = head.Y - tail.Y

    (x, y)

let moveTail head tail =
    let d = diff head tail

    match d with
    | (2, 0) -> { tail with X = tail.X + 1 }
    | (-2, 0) -> { tail with X = tail.X - 1 }
    | (0, 2) -> { tail with Y = tail.Y + 1 }
    | (0, -2) -> { tail with Y = tail.Y - 1 }
    | (1, 2) -> { tail with X = tail.X + 1; Y = tail.Y + 1 }
    | (-1, 2) -> { tail with X = tail.X - 1; Y = tail.Y + 1 }
    | (-1, -2) -> { tail with X = tail.X - 1; Y = tail.Y - 1 }
    | (1, -2) -> { tail with X = tail.X + 1; Y = tail.Y - 1 }
    | (2, -1) -> { tail with X = tail.X + 1; Y = tail.Y - 1 }
    | (2, 1) -> { tail with X = tail.X + 1; Y = tail.Y + 1 }
    | (-2, 1) -> { tail with X = tail.X - 1; Y = tail.Y + 1 }
    | (-2, -1) -> { tail with X = tail.X - 1; Y = tail.Y - 1 }
    | _ -> tail

let mover (state: State) count (headUpdater: (Point -> Point)) =
    seq { 0 .. count - 1 }
    |> Seq.fold (fun s v ->
        let newHead = headUpdater s.Head
        let newTail = moveTail newHead s.Tail

        { s with Head = newHead; Tail = newTail; TailPointsVisited = Set.add newTail s.TailPointsVisited }
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

let start = { Head = { X = 0; Y = 0 }; Tail = { X = 0; Y = 0 }; TailPointsVisited = Set.empty<Point> }

let result =
    System.IO.File.ReadAllLines(@"9/puzzle.txt")
    |> parseMoves
    |> Array.fold (fun s m -> applyMove s m ) start

//let result = applyMove start (Right 1)
//let result2 = applyMove result (Up 2)

//printState result

Set.count result.TailPointsVisited