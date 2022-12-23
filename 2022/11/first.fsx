type Monkey = {
    Number: int
    Items: int list
    Operation: int -> int
    TestValue: int
    TrueDestination: int
    FalseDestination: int
    TotalInspections: int
}

let parseMonkey (lines: string array) =
    let parseMonkeyNum str =
        let monkeyNumMatch = System.Text.RegularExpressions.Regex.Match(str, "^Monkey ([0-9]+):$")
        int (monkeyNumMatch.Groups[1].Value)
    
    let parseStartingItems (str: string) =
        let itemsStr = str.Substring(18)
        
        itemsStr.Split(", ")
        |> Array.map int
        |> Array.toList

    let parseOperation (str: string) =
        let opStr = str.Substring(19)

        let op =
            if opStr = "old * old" then (fun x -> x * x)
            elif opStr.StartsWith("old * ") then
                let intVal = int (opStr.Substring(6))
                
                (fun x -> x * intVal)
            elif opStr.StartsWith("old + ") then
                let intVal = int (opStr.Substring(6))

                (fun x -> x + intVal)
            else
                failwith "Error"

        op

    let parseTestValue (str: string) =
        str.Substring(21)
        |> int

    let parseTrueDestination (str: string) =
        str.Substring(29)
        |> int

    let parseFalseDestination (str: string) =
        str.Substring(30)
        |> int

    {
        Number = parseMonkeyNum lines[0]
        Items = parseStartingItems lines[1]
        Operation = parseOperation lines[2]
        TestValue = parseTestValue lines[3]
        TrueDestination = parseTrueDestination lines[4]
        FalseDestination = parseFalseDestination lines[5]
        TotalInspections = 0
    }

let processMonkey (monkeyMap: Map<int, Monkey>) (monkeyNum: int) =
    let passToMonkey monkeyNumber worryNumber (monkeyMap: Map<int, Monkey>) =
        Map.change monkeyNumber (fun monkeyOpt ->
            match monkeyOpt with
            | Some monkey -> Some { monkey with Items = List.append monkey.Items (List.singleton worryNumber) }
            | None -> None
        ) monkeyMap
    
    let processItem monkey worryNumber monkeyMap =
        let newWorry = 
            worryNumber
            |> monkey.Operation
            |> double
            |> (fun x -> x / 3.)
            |> floor
            |> int

        let destination =
            if (newWorry % monkey.TestValue) = 0 then monkey.TrueDestination
            else monkey.FalseDestination

        monkeyMap
        |> passToMonkey destination newWorry

    let monkey = Map.find monkeyNum monkeyMap
    let inspectionCount = List.length monkey.Items

    let newMap =
        List.fold (fun s i -> processItem monkey i s) monkeyMap monkey.Items
    
    Map.change monkey.Number (fun monkeyOpt ->
        match monkeyOpt with
        | Some monkey -> Some { monkey with Items = []; TotalInspections = monkey.TotalInspections + inspectionCount }
        | None -> None
        ) newMap

let processRound (monkeyMap: Map<int, Monkey>) =
    let monkeyCount = Map.count monkeyMap

    seq { 0 .. (monkeyCount - 1) }
    |> Seq.fold (fun s i -> processMonkey s i) monkeyMap

let result =
    System.IO.File.ReadAllLines("11/puzzle.txt")
    |> Array.chunkBySize 7
    |> Array.map parseMonkey
    |> Array.map (fun m -> m.Number, m)
    |> Map.ofArray
    |> (fun map ->
        seq { 1 .. 20 }
        |> Seq.fold (fun s _ -> processRound s) map
    )
    |> Map.map (fun _ v -> v.TotalInspections)
    |> Map.toList
    |> List.map snd
    |> List.sortDescending
    |> List.take 2
    |> List.pairwise
    |> List.head
    |> (fun (x, y) -> x * y)
