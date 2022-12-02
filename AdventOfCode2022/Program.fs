module Program

    open AdventOfCode2022.Puzzles

    [<EntryPoint>]
    let main _args =
        
        printfn "*** *** *** *** *** *** *** *** *** ***"
        printfn "***       Advent Of Code 2022       ***"
        printfn "*** *** *** *** *** *** *** *** *** ***"
        
        printfn "---------------- Day 1 ----------------"
        printfn $"** Part 1 : {Day1.part1()}"
        printfn $"** Part 2 : {Day1.part2()}"
        
        printfn "---------------- Day 2 ----------------"
        printfn $"** Part 1 : {Day2.part1()}"
        printfn $"** Part 2 : {Day2.part2()}"
        
        printfn "---------------- Day 3 ----------------"
        printfn $"** Part 1 : {Day3.part1()}"
        printfn $"** Part 2 : {Day3.part2()}"
        
        0