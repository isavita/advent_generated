
defmodule CaveSystem do
  def solve do
    {:ok, input} = File.read("input.txt")
    {depth, target} = parse_input(input)
    
    cave = make_cave_system(depth, target)
    calculate_risk_level(cave, target)
    |> IO.puts()
  end

  def parse_input(input) do
    [depth_line, target_line] = String.split(input, "\n")
    depth = depth_line |> String.split(" ") |> Enum.at(1) |> String.to_integer()
    
    [x, y] = target_line 
             |> String.split(" ") 
             |> Enum.at(1) 
             |> String.split(",")
             |> Enum.map(&String.to_integer/1)
    
    {depth, {x, y}}
  end

  def make_cave_system(depth, {target_x, target_y}) do
    for y <- 0..target_y, reduce: %{} do
      cave ->
        for x <- 0..target_x, reduce: cave do
          row ->
            geologic_index = cond do
              {x, y} == {0, 0} or {x, y} == {target_x, target_y} -> 0
              y == 0 -> x * 16807
              x == 0 -> y * 48271
              true -> Map.get(row, {x-1, y}) * Map.get(row, {x, y-1})
            end
            
            erosion_level = rem(geologic_index + depth, 20183)
            Map.put(row, {x, y}, erosion_level)
        end
    end
  end

  def calculate_risk_level(cave, {target_x, target_y}) do
    for y <- 0..target_y, x <- 0..target_x, reduce: 0 do
      risk -> risk + rem(Map.get(cave, {x, y}), 3)
    end
  end
end

CaveSystem.solve()
