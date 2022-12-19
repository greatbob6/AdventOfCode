
let parseLine cycles str =
    if str = "noop" then
        id :: cycles
    else
        let strVal = str.Substring(5)
        let intVal = int strVal

        List.append [ (fun x -> x + intVal); id; id ] cycles

let rec processor instructions cycleNum xVal collector =
    match instructions with
    | [] -> Map.add cycleNum xVal collector
    | instr :: rest ->
        if instr = "noop" then
            processor rest (cycleNum + 1) xVal (Map.add cycleNum xVal collector)
        else
            let change = int (instr.Substring(5))

            let firstCycle = Map.add cycleNum xVal collector
            let secondCycle = Map.add (cycleNum + 1) xVal firstCycle

            processor rest (cycleNum + 2) (xVal + change) secondCycle

let calculateSignalStrengths cycleValues =
    [ 20; 60; 100; 140; 180; 220 ]
    |> List.map (fun x -> x * (Map.find x cycleValues))
    |> List.sum

let result =
    System.IO.File.ReadAllLines("10/puzzle.txt")
    |> Array.toList
    |> (fun l -> processor l 1 1 Map.empty<int, int>)
    |> calculateSignalStrengths
