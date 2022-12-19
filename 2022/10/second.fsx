
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

let whichPix cycleValues cycle =
    let pixPos = (cycle - 1) % 40
    let v = Map.find cycle cycleValues

    if v >= pixPos - 1 && v <= pixPos + 1 then "#"
    else "."

let calculatePixels cycleValues =
    [ 1 .. 240 ]
    |> List.map (fun x -> whichPix cycleValues x)
    |> List.chunkBySize 40
    |> List.map (String.concat "")

let result =
    System.IO.File.ReadAllLines("10/puzzle.txt")
    |> Array.toList
    |> (fun l -> processor l 1 1 Map.empty<int, int>)
    |> calculatePixels

List.iter (fun (x: string) -> System.Console.WriteLine(x)) result

//let v = Map.find 200 result
//let pixPos = (200 - 1) % 40
//whichPix result 200
