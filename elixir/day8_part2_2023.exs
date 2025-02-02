
defmodule Day8 do
  def read_file(filename) do
    File.read!(filename)
    |> String.trim()
    |> String.split("\n")
  end

  def parse_input(input) do
    instructions = List.first(input)
    nodes =
      Enum.drop(input, 2)
      |> Enum.reduce(%{}, fn line, acc ->
        [head, children] = String.split(line, " = ")
        children =
          children
          |> String.replace(~r/[()]/, "")
          |> String.split(", ")
          |> List.to_tuple()

        Map.put(acc, head, children)
      end)

    %{instructions: instructions, nodes: nodes}
  end

  def gcd(a, 0), do: a
  def gcd(a, b), do: gcd(b, rem(a, b))

  def lcm(a, b), do: div(a * b, gcd(a, b))

  def lcm_list(nums) do
    Enum.reduce(nums, 1, fn num, acc -> lcm(acc, num) end)
  end

  def solve(input) do
    %{instructions: instructions, nodes: nodes} = parse_input(input)

    starts =
      Map.keys(nodes)
      |> Enum.filter(fn node -> String.ends_with?(node, "A") end)

    steps =
      Enum.map(starts, fn start ->
        walk_steps(start, instructions, nodes, 0)
      end)

    lcm_list(steps)
  end

  def walk_steps(current_node, instructions, nodes, steps) do
    if String.ends_with?(current_node, "Z") do
      steps
    else
      instruction = String.at(instructions, rem(steps, String.length(instructions)))
      {left, right} = nodes[current_node]

      next_node =
        case instruction do
          "L" -> left
          "R" -> right
        end
      walk_steps(next_node, instructions, nodes, steps + 1)
    end
  end

  def main() do
    input = read_file("input.txt")
    solve(input) |> IO.puts()
  end
end

Day8.main()
