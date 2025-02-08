
defmodule Garden do
  def read_map(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
  end

  def find_regions(map) do
    regions = %{}
    visited = MapSet.new()

    rows = length(map)
    cols = length(Enum.at(map, 0))

    Enum.reduce(0..(rows - 1), {regions, visited}, fn row, {regions, visited} ->
      Enum.reduce(0..(cols - 1), {regions, visited}, fn col, {regions, visited} ->
        if MapSet.member?(visited, {row, col}) do
          {regions, visited}
        else
          plant = Enum.at(Enum.at(map, row), col)
          {region_cells, new_visited} = traverse(map, row, col, plant, visited)
          new_regions = Map.update(regions, plant, [region_cells], &(&1 ++ [region_cells]))
          {new_regions, new_visited}
        end
      end)
    end)
    |> elem(0)
  end


  def traverse(map, row, col, plant, visited) do
    rows = length(map)
    cols = length(Enum.at(map, 0))

    cond do
      row < 0 or row >= rows or col < 0 or col >= cols ->
        {[], visited}
      MapSet.member?(visited, {row, col}) ->
        {[], visited}
      Enum.at(Enum.at(map, row), col) != plant ->
        {[], visited}
      true ->
        new_visited = MapSet.put(visited, {row, col})
        {{row, col}, new_visited} = {{row, col}, new_visited}

        {up, visited_up} = traverse(map, row - 1, col, plant, new_visited)
        {down, visited_down} = traverse(map, row + 1, col, plant, visited_up)
        {left, visited_left} = traverse(map, row, col - 1, plant, visited_down)
        {right, visited_right} = traverse(map, row, col + 1, plant, visited_left)

        {[{row, col} | (up ++ down ++ left ++ right)], visited_right}
    end
  end


  def calculate_perimeter(map, region_cells) do
     rows = length(map)
     cols = length(Enum.at(map,0))

     Enum.reduce(region_cells, 0, fn {row, col}, perimeter ->
        neighbors = [
          {row - 1, col},
          {row + 1, col},
          {row, col - 1},
          {row, col + 1}
        ]

        perimeter + Enum.reduce(neighbors, 0, fn {nr, nc}, acc ->
          cond do
             nr < 0 or nr >= rows or nc < 0 or nc >= cols -> acc + 1
             not Enum.member?(region_cells, {nr, nc}) -> acc + 1
             true -> acc
          end
        end)
     end)
  end


  def calculate_total_price(filename) do
    map = read_map(filename)
    regions_by_plant = find_regions(map)

    Enum.reduce(regions_by_plant, 0, fn {_plant, regions}, total_price ->
      Enum.reduce(regions, total_price, fn region_cells, acc ->
        area = length(region_cells)
        perimeter = calculate_perimeter(map, MapSet.new(region_cells))
        acc + area * perimeter
      end)
    end)
  end
end

Garden.calculate_total_price("input.txt") |> IO.puts()
