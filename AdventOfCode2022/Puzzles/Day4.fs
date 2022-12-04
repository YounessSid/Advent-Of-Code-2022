module AdventOfCode2022.Puzzles.Day4

    open AdventOfCode2022.Inputs
    
    type Section = {
        From: int
        Until: int
    }
    type Pair = {
        Section1: Section
        Section2: Section
    }
    module Pair =
        let stringToSection (s: string) : Section =
            let split = s.Split("-")
            
            {
              From = split |> Array.item(0) |> int
              Until = split |> Array.item(1) |> int
            }
            
        let fromString (s: string) : Pair =
            let split = s.Split(",")
            {
                Section1 = split |> Array.item(0) |> stringToSection
                Section2 = split |> Array.item(1) |> stringToSection
            }
            
        let doesOneSectionFullyContainTheOther (pair: Pair) =
            let section1ContainsSection2 =
                pair.Section2.From >= pair.Section1.From && pair.Section2.Until <= pair.Section1.Until
            
            let section2ContainsSection1 =
                pair.Section1.From >= pair.Section2.From && pair.Section1.Until <= pair.Section2.Until
                
            if (section1ContainsSection2 || section2ContainsSection1) then Some pair else None
            
        let isThereAnOverlapAtAll (pair: Pair) =
            let sectionOneRange =
                [| pair.Section1.From .. pair.Section1.Until |] |> Set.ofArray
            
            let sectionTwoRange =
                [| pair.Section2.From .. pair.Section2.Until |] |> Set.ofArray
            
            Set.intersect sectionOneRange sectionTwoRange
            |> Seq.length > 0
            |> fun x -> match x with true -> Some pair | false -> None
            
    let dayFourInput = FileReader.readDay(4)
    let parsedPairs =
        dayFourInput
        |> Seq.map Pair.fromString
    let part1 _ =
        parsedPairs
        |> Seq.map Pair.doesOneSectionFullyContainTheOther
        |> Seq.choose id
        |> Seq.length
        
    let part2 _ =
        parsedPairs
        |> Seq.map Pair.isThereAnOverlapAtAll
        |> Seq.choose id
        |> Seq.length