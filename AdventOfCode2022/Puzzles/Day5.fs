module AdventOfCode2022.Puzzles.Day5

    open System
    open Microsoft.FSharp.Core
    open AdventOfCode2022.Inputs
    open System.Text.RegularExpressions
    
    type Container = string []
    
    type Instruction = {
        Move    : int
        From    : int
        To      : int
    }
    
    module Instruction =
        let fromString (s: string) =
            let sniffNumbers = Regex.Replace(s, "[^0-9 _]", "")
            let instructions =
                sniffNumbers.Trim().Split(" ")
                |> Array.filter(fun x -> not <| String.IsNullOrWhiteSpace(x))
            
            {
                Move = instructions |> Array.item(0) |> int
                From = instructions |> Array.item(1) |> int
                To = instructions |> Array.item(2) |> int
            }

        let applyWithNormalCrane (instruction: Instruction) (containers: Container []) : Container [] =
            
            let moveOneTime containers =
                match containers |> Array.tryItem(instruction.From - 1), containers |> Array.tryItem(instruction.To - 1) with
                | Some fromContainer, Some toContainer ->
                    let elementToMove = fromContainer |> Array.tryHead
                    match elementToMove with
                    | Some head ->
                        
                        let updatedToContainer = Array.append [| head |] toContainer
                        let updatedFromContainer =
                            try fromContainer |> Array.tail
                            with
                            | _ -> [|  |]
                        
                        containers
                        |> Array.mapi(fun index container ->
                            if index.Equals(instruction.From - 1) then updatedFromContainer
                            elif index.Equals(instruction.To - 1) then updatedToContainer
                            else container
                        )
                        
                    | None -> containers
                    
                | _ -> containers
            
            [| 1 .. instruction.Move |]
            |> Array.fold (fun state _ -> moveOneTime state) containers
        
        let applyWithAdvancedCrane (instruction: Instruction) (containers: Container []) : Container [] =
            match containers |> Array.tryItem(instruction.From - 1), containers |> Array.tryItem(instruction.To - 1) with
            | Some fromContainer, Some toContainer ->
                
                let multipleCrates = fromContainer |> Array.take(instruction.Move)
                let updatedToContainer = Array.append multipleCrates toContainer
                let updatedFromContainer = Array.sub fromContainer instruction.Move (fromContainer.Length - instruction.Move)

                containers
                |> Array.mapi(fun index container ->
                    if index.Equals(instruction.From - 1) then updatedFromContainer
                    elif index.Equals(instruction.To - 1) then updatedToContainer
                    else container
                )
    
            | _ -> containers
            
    
    let dayFiveInput = FileReader.readDay(5)
    
    let numberOfContainers =
        dayFiveInput
        |> Seq.filter (fun x -> x.Trim().StartsWith("1"))
        |> String.concat("")
        |> fun x -> x.Trim().Split(" ")
        |> Seq.filter (fun x -> not <| String.IsNullOrWhiteSpace(x))
        |> Seq.map int
        |> Seq.max
    
    let spaceIndexes =
        [|1 .. (numberOfContainers - 1)|]
        |> Array.fold (fun (state: int[]) (item: int) ->  [|(4 * item) - 1|] |> Array.append state)[|  |]
        
    let containers : Container [] =
        let cratesMatrix =
            dayFiveInput
            |> Seq.filter (fun x ->
                not <| String.IsNullOrWhiteSpace(x)
                && not <| x.Trim().StartsWith("move")
                && not <| x.Trim().StartsWith("1"))
            |> Seq.map(fun row ->
                row.ToCharArray()
                |> Seq.mapi(fun index x -> if spaceIndexes |> Array.contains index then "|" else $"{x}")
                |> String.concat("")
                |> fun x -> x.Replace("   ", "[0]"))
            |> Seq.map (fun row -> row.Split("|"))
        
        [| 0 .. (numberOfContainers - 1) |]
        |> Array.map (fun index ->
            cratesMatrix
            |> Seq.fold (fun state item -> [| item |> Array.item(index) |] |> Array.append state) [|  |])
        |> Array.map (fun container -> container |> Array.filter (fun x -> not <| x.Equals("[0]")))

    let instructions =
        dayFiveInput
        |> Seq.filter (fun x -> x.StartsWith("move"))
        |> Seq.map Instruction.fromString
    
    type Machine = CrateMover9000 | CrateMover9001
    
    let startLiftingWith advancedMachine =
        let liftingTechnique =
            match advancedMachine with
            | CrateMover9000 -> Instruction.applyWithNormalCrane
            | CrateMover9001 -> Instruction.applyWithAdvancedCrane
            
        instructions
        |> Seq.fold (fun state instruction -> liftingTechnique instruction state) containers
        |> Array.map (fun container -> container |> Array.head)
        |> String.concat("")
        |> fun x -> x.Replace("[", "").Replace("]", "")
    
    let part1 _ = startLiftingWith CrateMover9000

    let part2 _ = startLiftingWith CrateMover9001