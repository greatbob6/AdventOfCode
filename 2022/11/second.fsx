type Monkey = {
    Number: int
    Items: int list
    Operation: int -> int
    TestValue: int
    TrueDestination: int
    FalseDestination: int
    TotalInspections: decimal
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
        TotalInspections = 0M
    }

let processMonkey worryAdjuster (monkeyMap: Map<int, Monkey>) (monkeyNum: int) =
    let passToMonkey monkeyNumber worryNumber (monkeyMap: Map<int, Monkey>) =
        Map.change monkeyNumber (fun monkeyOpt ->
            match monkeyOpt with
            | Some monkey -> Some { monkey with Items = List.append monkey.Items (List.singleton worryNumber) }
            | None -> None
        ) monkeyMap
    
    let processItem worryAdjuster monkey worryNumber monkeyMap =
        let newWorry = 
            worryNumber
            |> monkey.Operation
            //|> double
            |> (fun x -> x % worryAdjuster)
            //|> floor
            //|> int

        let isTrue = (newWorry % monkey.TestValue) = 0

        let destination =
            if isTrue then monkey.TrueDestination
            else monkey.FalseDestination

        //System.Console.WriteLine($"{monkey.Number}:{worryNumber}:{isTrue}:{destination}")

        monkeyMap
        |> passToMonkey destination newWorry

    let monkey = Map.find monkeyNum monkeyMap
    let inspectionCount = List.length monkey.Items

    let newMap =
        List.fold (fun s i -> processItem worryAdjuster monkey i s) monkeyMap monkey.Items
    
    Map.change monkey.Number (fun monkeyOpt ->
        match monkeyOpt with
        | Some monkey -> Some { monkey with Items = []; TotalInspections = monkey.TotalInspections + (decimal inspectionCount) }
        | None -> None
        ) newMap

let processRound worryAdjuster (monkeyMap: Map<int, Monkey>) =
    let monkeyCount = Map.count monkeyMap

    seq { 0 .. (monkeyCount - 1) }
    |> Seq.fold (fun s i -> processMonkey worryAdjuster s i) monkeyMap

let rec gcd a b =
    if a <> 0 then (gcd (b % a) a) else b

// least-common multiple of 2 numbers
let lcm a b = (a * b) / (gcd a b);

let result =
    System.IO.File.ReadAllLines("11/test.txt")
    |> Array.chunkBySize 7
    |> Array.map parseMonkey
    |> Array.map (fun m -> m.Number, m)
    |> Map.ofArray
    |> (fun map ->
        let worryAdjuster =
            map
            |> Map.map (fun _ m -> m.TestValue)
            |> Map.toList
            |> List.map snd
            |> List.reduce lcm

        //System.Console.WriteLine($"Worry Adjuster: {worryAdjuster}")

        seq { 1 .. 20 }
        |> Seq.fold (fun s _ -> processRound worryAdjuster s) map
    )
    |> Map.map (fun _ v -> v.TotalInspections)
    //|> Map.toList
    //|> List.map snd
    //|> List.sortDescending
    //|> List.take 2
    //|> List.pairwise
    //|> List.head
    //|> (fun (x, y) -> x * y)
