module AdventOfCode2022.Inputs.FileReader

    open System.IO
    
    let readDay (day: int) =
        File.ReadLines($"Inputs/Day{day}.txt")
        |> Seq.map string
