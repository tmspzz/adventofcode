require File
require IO

defmodule Main do

  # Part 1
  def totalFuel() do
    case File.read "input.txt" do
      {:ok, contents} -> 
    modulesMasses = contents 
    |> String.split("\n", trim: true)
        |> Enum.map(&String.to_integer/1)
    
    modulesMasses 
        |> Enum.reduce(0, fn(mass, acc) -> acc + fuelForModuleWithMass mass end)
        |> fn total -> IO.puts "Total fuel before fuel mass addition: #{total}" end.()
        
    modulesMasses 
        |> Enum.reduce(0, fn(mass, acc) -> acc + fuelForModuleWithMassAccountingForFuelMass mass end)
        |> fn total -> IO.puts "Total fuel after fuel mass addition: #{total}" end.()
      {:error, reason} ->
    IO.puts("Cannot open input.txt: #{reason}")
    end
  end
    
  def fuelForModuleWithMass(mass) do
    div(mass, 3) - 2
  end
  
   # Part 2
   def fuelForModuleWithMassAccountingForFuelMass(mass) do
    massForMudule = fuelForModuleWithMass mass
     if massForMudule > 0 do
      massForMudule + fuelForModuleWithMassAccountingForFuelMass massForMudule
     else
       0
     end
   end  

end
