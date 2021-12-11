<Query Kind="FSharpProgram" />

type CountResult = {
    TotalDescendents: int64
    Cache: Map<int64 * int64, int64>
}

let childDays days start n =
    (days - (7L * n) - (start + 1L)) - 2L

//let mapMerge = Map.fold (fun acc key value -> Map.add key value acc)

let rec childrenSpawnCount (cache: Dictionary<int, int64>) days t =
    let cacheKey = (int days) * 10 + (int t)
    
    if cache.ContainsKey(cacheKey) then cache[cacheKey]
    else
        let childCount =
            (float (days - t) / 7.)
            |> ceil
            |> int64
            |> (max 0L)
        
        //$"days: {days}, t: {t}, childCount: {childCount}".Dump()
        
        if childCount > 0L then
            let res =
                seq { 0L .. (childCount - 1L) }
                |> Seq.map (fun n -> childrenSpawnCount cache (childDays days t n) 6L)
                |> Seq.sum
                |> (+) childCount
            
            cache[cacheKey] <- res
            
            res
            
        else childCount
    
let fish = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\6\puzzle_input.txt")
    |> Array.head
    |> (fun s -> s.Split(","))
    |> Array.map int64

let days = 256L

let cache = new Dictionary<int, int64>()

let childrenSpawned =
    fish
    |> Array.map (childrenSpawnCount cache days)
    |> Array.sum
    |> (+) (int64 <| (Array.length fish))
    
childrenSpawned.Dump()
