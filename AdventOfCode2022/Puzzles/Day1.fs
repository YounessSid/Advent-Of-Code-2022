module AdventOfCode2022.Puzzles.Day1

    open AdventOfCode2022.Inputs
    
    let dayOneInput = FileReader.readDay(1)
    
    let totalCaloriesPerElf =
        (* Gives back an array representing the sum of calories for every Elf *)
        dayOneInput
        |> String.concat(",")
        |> fun x -> x.Split(",,")
        |> Array.map(fun x -> x.Split(",") |> (Array.map int >> Array.sum))
    
    let part1 _ =
        (* Gets the biggest amount of calories gathered by some Elf *)
        totalCaloriesPerElf
        |> Array.max
        
    let part2 _ =
        (* Gets the top three biggest amounts of calories gathered and sums them *)
        totalCaloriesPerElf
        |> Array.sortDescending
        |> Array.take(3)
        |> Array.sum