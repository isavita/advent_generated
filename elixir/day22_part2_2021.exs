
defmodule Solution do
  def solve(input) do
    cubes = parse_input(input)
    process_cubes(cubes, [])
  end

  def process_cubes([], final_list), do: calculate_volume(final_list)
  def process_cubes([current | rest], final_list) do
    to_add = 
      Enum.flat_map(final_list, fn final_cube ->
        case get_intersection(final_cube, current) do
          {:ok, intersection} -> [intersection]
          :no_intersection -> []
        end
      end)

    final_list = 
      if current.is_on do
        final_list ++ to_add ++ [current]
      else
        final_list ++ to_add
      end

    process_cubes(rest, final_list)
  end

  def get_intersection(c1, c2) do
    x1 = max(c1.x1, c2.x1)
    x2 = min(c1.x2, c2.x2)
    y1 = max(c1.y1, c2.y1)
    y2 = min(c1.y2, c2.y2)
    z1 = max(c1.z1, c2.z1)
    z2 = min(c1.z2, c2.z2)

    cond do
      x1 > x2 or y1 > y2 or z1 > z2 -> 
        :no_intersection
      true ->
        is_on = 
          cond do
            c1.is_on and c2.is_on -> false
            not c1.is_on and not c2.is_on -> true
            true -> c2.is_on
          end

        {:ok, %{is_on: is_on, x1: x1, x2: x2, y1: y1, y2: y2, z1: z1, z2: z2}}
    end
  end

  def calculate_volume(cubes) do
    Enum.reduce(cubes, 0, fn cube, acc ->
      volume = (cube.x2 - cube.x1 + 1) * (cube.y2 - cube.y1 + 1) * (cube.z2 - cube.z1 + 1)
      acc + if cube.is_on, do: volume, else: -volume
    end)
  end

  def parse_input(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(fn line ->
      [state, coords] = String.split(line, " ")
      [x, y, z] = String.split(coords, ",")

      [x1, x2] = 
        x 
        |> String.slice(2..-1) 
        |> String.split("..")
        |> Enum.map(&String.to_integer/1)

      [y1, y2] = 
        y 
        |> String.slice(2..-1) 
        |> String.split("..")
        |> Enum.map(&String.to_integer/1)

      [z1, z2] = 
        z 
        |> String.slice(2..-1) 
        |> String.split("..")
        |> Enum.map(&String.to_integer/1)

      %{
        is_on: state == "on",
        x1: x1, x2: x2,
        y1: y1, y2: y2,
        z1: z1, z2: z2
      }
    end)
  end

  def main do
    {:ok, input} = File.read("input.txt")
    result = solve(input)
    IO.puts(result)
  end
end

Solution.main()
