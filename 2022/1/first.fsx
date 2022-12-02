open System

let lines = System.IO.File.ReadAllLines("./2022/1/puzzle.txt")

type State =
    {
        CurrentElf: int list
        ElfList: int list list
    }

let result =
    lines
    |> Array.fold (fun state x -> if not(String.IsNullOrEmpty(x)) then { state with CurrentElf = int(x) :: state.CurrentElf } else {state with CurrentElf = []; ElfList = state.CurrentElf :: state.ElfList } ) { CurrentElf = []; ElfList = [] }
    |> (fun s -> if List.isEmpty s.CurrentElf then s.ElfList else s.CurrentElf :: s.ElfList)
    |> List.rev

let calorieCounts =
    result
    |> List.mapi (fun idx entries -> idx, List.sum entries)

let maxElf = List.maxBy snd calorieCounts
