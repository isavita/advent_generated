
defmodule Solution do
  def solve do
    {:ok, input} = File.read("input.txt")
    
    [instructions | map_lines] = String.split(input, "\n", trim: true)
    
    desert_map = 
      map_lines
      |> Enum.drop(1)
      |> Enum.reduce(%{}, fn line, acc ->
        [start, left, right] = Regex.run(~r/([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)/, line, capture: :all_but_first)
        Map.put(acc, start, {left, right})
      end)
    
    instructions = String.graphemes(instructions)
    
    do_navigate("AAA", instructions, desert_map, instructions, 0)
  end
  
  defp do_navigate("ZZZ", _, _, _, steps), do: steps
  
  defp do_navigate(current, [], map, full_instructions, steps) do
    do_navigate(current, full_instructions, map, full_instructions, steps)
  end
  
  defp do_navigate(current, [instruction | rest], map, full_instructions, steps) do
    next = 
      case instruction do
        "R" -> elem(map[current], 1)
        "L" -> elem(map[current], 0)
      end
    
    do_navigate(next, rest, map, full_instructions, steps + 1)
  end
end

Solution.solve() |> IO.puts()
