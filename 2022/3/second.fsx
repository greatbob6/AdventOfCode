type ElfGroup = {
    First: string
    Second: string
    Third: string
}

let parseElfGroup (str:string[]) =
    { First = str[0]; Second = str[1]; Third = str[2] }

let findCommonItem group =
    let firstSet = Set.ofArray (group.First.ToCharArray())
    let secondSet = Set.ofArray (group.Second.ToCharArray())
    let thirdSet = Set.ofArray (group.Third.ToCharArray())

    let intersect = Set.intersectMany (seq { firstSet; secondSet; thirdSet })

    (Set.toArray intersect)[0]

let calculatePriority item =
    if item >= 'a' && item <= 'z' then int(item) - int('a') + 1
    elif item >= 'A' && item <= 'Z' then int(item) - int('A') + 27
    else 0

let results =
    System.IO.File.ReadAllLines("3/puzzle.txt")
    |> Array.chunkBySize 3
    |> Array.map parseElfGroup
    |> Array.map findCommonItem
    |> Array.map calculatePriority
    |> Array.sum

