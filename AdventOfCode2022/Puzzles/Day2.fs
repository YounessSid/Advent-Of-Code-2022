module AdventOfCode2022.Puzzles.Day2

    open AdventOfCode2022.Inputs

    (* Models *)
    type Player = Opponent | Me
    type OpponentChoice = A | B | C
    module OpponentChoice =
        let fromString(s: string) =
            match s with
            | "A" -> A
            | "B" -> B
            | "C" -> C
            | other -> failwithf $"Failed to parse opponent's choice : '{other}'"
    type MyChoice = X | Y | Z
    module MyChoice =
        let fromString(s: string) =
            match s with
            | "X" -> X
            | "Y" -> Y
            | "Z" -> Z
            | other -> failwithf $"Failed to parse my choice: '{other}'"     
    type RoundResult =
        | Win   of Player
        | Draw
    
    (* Helpers *)
    let whoWinsTheRound inputCombination =
        match inputCombination with
        | A, X
        | B, Y
        | C, Z -> Draw
        
        | A, Y
        | B, Z 
        | C, X -> Win Me
        
        | A, Z 
        | B, X 
        | C, Y -> Win Opponent
        
    let myScorePerChoice choice =
        match choice with
        | X -> 1
        | Y -> 2
        | Z -> 3
    
    let myScorePerResult result =
        match result with
        | Win Opponent  -> 0
        | Draw          -> 3
        | Win Me        -> 6
    
    let calculateMyTotalRoundScore inputCombination =
        let myChoice = snd inputCombination
        let roundResult = whoWinsTheRound inputCombination
        
        myScorePerChoice myChoice + myScorePerResult roundResult
    
    (* Puzzles *)    
    let dayTwoInput = FileReader.readDay(2)
    let part1 _ =
        dayTwoInput
        |> Seq.map(fun x -> x.Split(" "))
        |> Seq.map(fun x -> OpponentChoice.fromString(x |> Array.item(0)), MyChoice.fromString(x |> Array.item(1)))
        |> Seq.map calculateMyTotalRoundScore
        |> Seq.sum
        
    let part2 _ = "Not yet implemented"