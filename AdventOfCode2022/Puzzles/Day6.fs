module AdventOfCode2022.Puzzles.Day6

    open AdventOfCode2022.Inputs
    
    let day6input =
        FileReader.readDay(6)
        |> String.concat("")
        |> fun x -> x.ToCharArray()
    
    let tryFindItem index = day6input |> Array.tryItem index
    
    let findUniqueCharsSection startIndex numberOfUniqueChars =
        if startIndex < numberOfUniqueChars then None
        else
            [ 0 .. (startIndex - 1) ]
            |> List.take numberOfUniqueChars
            |> List.map (fun i -> startIndex - i)
            |> List.map tryFindItem
            |> List.choose id
            |> List.distinct
            |> fun x -> if x.Length = numberOfUniqueChars then Some x else None
            
    let getMarkerPosition numberOfUniqueChars =
        day6input
        |> Array.mapi (fun index _ ->
            match findUniqueCharsSection index numberOfUniqueChars with
            | Some _ -> Some index
            | None -> None)
        |> Array.choose id
        |> Array.head
        |> fun x -> x + 1
        
    let part1 _ = getMarkerPosition 4
        
    let part2 _ = getMarkerPosition 14