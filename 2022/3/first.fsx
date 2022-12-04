type Satchel = {
    First: string
    Second: string
}

let parseSatchel (str:string) =
    let itemCount = str.Length / 2
    let first = str[..itemCount - 1]
    let second = str[itemCount..]

    { First = first; Second = second }

let findCommonItem satchel =
    let firstSet = Set.ofArray (satchel.First.ToCharArray())
    let secondSet = Set.ofArray (satchel.Second.ToCharArray())

    let intersect = Set.intersect firstSet secondSet

    (Set.toArray intersect)[0]

let calculatePriority item =
    if item >= 'a' && item <= 'z' then int(item) - int('a') + 1
    elif item >= 'A' && item <= 'Z' then int(item) - int('A') + 27
    else 0


let results =
    System.IO.File.ReadAllLines("3/puzzle.txt")
    |> Array.map parseSatchel
    |> Array.map findCommonItem
    |> Array.map calculatePriority
    |> Array.sum

