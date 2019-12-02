require File
require IO

defmodule Main do

    # Part 1
    def runIntsProgram(verb \\ 12, noun \\ 2) do
        case File.read "input.txt" do
            {:ok, contents} -> 
                contents
                    |> String.trim 
                    |> String.split(",", trim: true)
                    |> Enum.map(&String.to_integer/1)
                    |> fn l ->
                            restoredMemory = List.replace_at(l, 1, verb) |> List.replace_at(2, noun)
                            runInstructions(restoredMemory, restoredMemory) end.()
        
            {:error, reason} ->
                IO.puts("Cannot open input.txt: #{reason}")
        end
    end

    # Part 2
    def findNounAndVerb() do
        case File.read "input.txt" do
            {:ok, contents} -> 
                contents
                    |> String.trim 
                    |> String.split(",", trim: true)
                    |> Enum.map(&String.to_integer/1)
                    |> fn l ->
                            for verb <- 0..99,
                                noun <- 0..99 do
                                restoredMemory = List.replace_at(l, 1, noun) |> List.replace_at(2, verb)
                                result = List.first(runInstructions(restoredMemory, restoredMemory))
                                if result === 19690720 do
                                    IO.puts("Found it! #{100 * noun + verb}")
                                end
                                result
                            end 
                       end.()
        
            {:error, reason} ->
                IO.puts("Cannot open input.txt: #{reason}")
        end
    end 

    def test() do
        program = [1,9,10,3,2,3,11,0,99,30,40,50]
        memory = program 
        IO.inspect(runInstructions(program, memory))
    end
    
    def runInstructions([], memory), do: memory 
    def runInstructions([opcode | tail], memory) do
        case opcode do
            1 -> 
                [operand1Index, operand2Index, storeIndex] = Enum.take(tail, 3)
                operand1 = Enum.at(memory, operand1Index)
                operand2 = Enum.at(memory, operand2Index)
                alteredMemory = List.replace_at(memory, storeIndex, operand1 + operand2)
                program = Enum.drop(tail, 3) 
                runInstructions(program, alteredMemory)
            2 ->
                [operand1Index, operand2Index, storeIndex] = Enum.take(tail, 3)
                operand1 = Enum.at(memory, operand1Index)
                operand2 = Enum.at(memory, operand2Index)
                alteredMemory = List.replace_at(memory, storeIndex , operand1 * operand2)
                program = Enum.drop(tail, 3) 
                runInstructions(program, alteredMemory)
            99 ->
                memory
        end
    end    
     
end
