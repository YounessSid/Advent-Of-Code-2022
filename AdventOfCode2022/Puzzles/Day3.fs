module AdventOfCode2022.Puzzles.Day3

    open AdventOfCode2022.Inputs
    type Rucksack = {
        Compartment1 : char[]
        Compartment2 : char[]
    }
    module Rucksack =
        let fromString (s: string) =
            let charArray = s.ToCharArray()
            
            charArray
            |> Array.splitAt(charArray.Length / 2)
            |> fun (firstHalf, secondHalf) -> { Compartment1 = firstHalf; Compartment2 = secondHalf }
        
        let findCommonItem (rucksack: Rucksack) =
            let compartment1 = rucksack.Compartment1 |> Set.ofArray
            let compartment2 = rucksack.Compartment2 |> Set.ofArray
            
            Set.intersect compartment1 compartment2
            |> Set.maxElement
        
        let findCommonItemInGroup (rucksacks: Rucksack[]) =
            rucksacks
            |> Array.map(fun rucksack -> Set.ofArray <| Array.append rucksack.Compartment1 rucksack.Compartment2)
            |> Array.toSeq
            |> Set.intersectMany
            |> Set.maxElement
            
    module Score =
        let characters = [|
            'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'
            'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'
            's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'A'
            'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'
            'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'
            'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'            
        |]
        let calculate(c: char) =
            characters
            |> Array.findIndex(fun x -> x.Equals(c))
            |> fun index -> index + 1
    
    let dayThreeInput = FileReader.readDay(3)
    
    let part1 _ =
        dayThreeInput
        |> Seq.map (Rucksack.fromString >> Rucksack.findCommonItem >> Score.calculate) 
        |> Seq.sum

    let part2 _ =
        dayThreeInput
        |> Seq.map Rucksack.fromString
        |> Seq.chunkBySize(3)
        |> Seq.map (Rucksack.findCommonItemInGroup >> Score.calculate)
        |> Seq.sum