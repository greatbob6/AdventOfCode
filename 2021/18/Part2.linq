<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

type ActionResult =
| NoActionTaken
| Exploded
| Split

type Node =
| Value of int
| Pair of Pair

and
    Pair = {
        Left: Node ref
        Right: Node ref
    }

type ParseResult = {
    Node: Node ref
    Remaining: char list
}

let rec parseNode chars =
    match chars with
    | [] -> failwith "Chars shouldn't be empty here"
    | char :: tail ->
        if char = '[' then // is opening char
            parsePair tail
        else
            parseValue chars
and
    parseValue chars =
        let endIdx = chars |> List.findIndex (fun c -> c = ',' || c = ']')
        
        let value = chars[..endIdx-1]
        
        {
            Node = ref (Value (value |> List.toArray |> String |> int))
            Remaining = chars[endIdx..]
        }
        
and
    parsePair chars =
        let leftResult = parseNode chars
        let rightResult = parseNode (List.tail leftResult.Remaining)
        
        let newNode = ref (Pair { Left = leftResult.Node; Right = rightResult.Node })
        
        {
            Node = newNode
            Remaining = List.tail rightResult.Remaining
        }

let rec treeToString (node: Node ref) =
    match node.Value with
    | Value v -> v.ToString()
    | Pair p ->
        let leftStr = treeToString p.Left
        let rightStr = treeToString p.Right
        
        $"[{leftStr},{rightStr}]"

let rec makeLeafList leaves (node: Node ref) =
    match node.Value with
    | Value _ ->
        node :: leaves
    | Pair p ->
        let withLeft = makeLeafList leaves p.Left
        
        makeLeafList withLeft p.Right

let rec makeLeafListWithTarget leaves (node: Node ref) target =
    if LanguagePrimitives.PhysicalEquality target node then
        node :: leaves
    else
        match node.Value with
        | Value _ ->
            node :: leaves
        | Pair p ->
            let withLeft = makeLeafListWithTarget leaves p.Left target
            
            makeLeafListWithTarget withLeft p.Right target
        
let getValue (nodeRef: Node ref) =
    match nodeRef.Value with
    | Value v -> v
    | _ -> failwith "BAD"

let getPairValues (nodeRef: Node ref) =
    match nodeRef.Value with
    | Pair p -> (getValue p.Left, getValue p.Right)
    | _ -> failwith "BAD"

let explode (top: Node ref) =
    let rec findExplodee level (node: Node ref) =
        match node.Value with
        | Pair p ->
            if level < 4 then
                let leftExplodee = findExplodee (level + 1) p.Left
                
                if Option.isSome leftExplodee then
                    leftExplodee
                else
                    findExplodee (level + 1) p.Right
            else
                let leftMatch =
                    match p.Left.Value with
                    | Pair _ -> Some p.Left
                    | _ -> None
                 
                if Option.isSome leftMatch then
                    leftMatch
                else
                    match p.Right.Value with
                    | Pair _ -> Some p.Right
                    | _ -> None
                
        | Value _ -> None
            
    let explodee = findExplodee 1 top
    
    match explodee with
    | None -> NoActionTaken
    | Some explodeeVal ->
        
        let leafList =
            makeLeafListWithTarget [] top explodeeVal
            |> List.rev
        
        let explodeeIdx = List.findIndex ((=) explodeeVal) leafList
        
        let front = leafList[..explodeeIdx-1]
        let back = leafList[explodeeIdx+1..]
        
        let leftNode = if List.isEmpty front then None else Some (List.last front)
        let rightNode = if List.isEmpty back then None else Some (List.head back)
        
        let leftVal, rightVal = getPairValues explodeeVal
        
        match leftNode with
        | None -> ()
        | Some node ->
            let prevVal = getValue node
            
            node.Value <- Value (prevVal + leftVal)
            
        match rightNode with
        | None -> ()
        | Some node ->
            let nextVal = getValue node
            
            node.Value <- Value (nextVal + rightVal)
        
        explodeeVal.Value <- Value 0
    
        Exploded
        
let split (top: Node ref) =
    let rec findSplit (node: Node ref) =
        makeLeafList List.empty node
        |> List.choose (fun x ->
            match x.Value with
            | Value _ -> Some x
            | _ -> None
        )
        |> List.filter (fun x -> (getValue x) >= 10)
        |> List.tryLast  // leaf list is reversed so first is actually last
            
    let splitee = findSplit top
    
    match splitee with
    | None -> NoActionTaken
    | Some node ->
        let curVal = getValue node
        let newLeft = (float curVal) / 2. |> floor |> int
        let newRight = (float curVal) / 2. |> ceil |> int
        
        node.Value <- Pair { Left = ref (Value newLeft); Right = ref (Value newRight) }
        
        Split
        
let rec reduce (top: Node ref) =
    let result = explode top
    
    match result with
    | Exploded -> reduce top
    | _ ->
        let splitResult = split top
        
        match splitResult with
        | Split -> reduce top
        | _ -> splitResult

let addNodes first second =
    let result = ref (Pair { Left = first; Right = second })
    
    reduce result |> ignore
    
    result
    
let rec calculateMagnitude (node: Node ref) =
    match node.Value with
    | Value v -> v
    | Pair p ->
        let leftMag = calculateMagnitude p.Left
        let rightMag = calculateMagnitude p.Right
        
        (leftMag * 3) + (rightMag * 2)

let treeData = 
    getData "puzzle"
    //|> Array.skip 0
    //|> Array.take 1
    //|> Array.head
    |> Array.map (fun line -> line.ToCharArray() |> List.ofArray)

let result =
    Seq.allPairs [0..(Array.length treeData)-1] [0..(Array.length treeData)-1]
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.map (fun (a, b) ->
        let treeA = (parseNode treeData[a]).Node
        let treeB = (parseNode treeData[b]).Node
        let mag = (addNodes treeA treeB) |> calculateMagnitude
        
        mag
    )
    |> Seq.sortDescending
    |> Seq.max

result.Dump()