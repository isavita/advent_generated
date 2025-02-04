
defmodule Solution do
  def solve do
    input = File.read!("input.txt")

    regex = ~r/mul\([0-9]{1,3},[0-9]{1,3}\)|do\(\)|don't\(\)/

    matches =
      Regex.scan(regex, input, return: :index)
      |> Enum.map(fn [{start, length}] -> {start, start + length} end)

    {total_sum, _} =
      Enum.reduce(matches, {0, true}, fn {start, stop}, {acc, enabled} ->
        instruction = String.slice(input, start, stop - start)

        cond do
          String.starts_with?(instruction, "mul") ->
            if enabled do
              [x, y] =
                instruction
                |> String.slice(4, String.length(instruction) - 5)
                |> String.split(",")
                |> Enum.map(&String.to_integer/1)

              {acc + x * y, enabled}
            else
              {acc, enabled}
            end

          instruction == "do()" ->
            {acc, true}

          instruction == "don't()" ->
            {acc, false}

          true ->
            {acc, enabled}
        end
      end)

    IO.puts(total_sum)
  end
end

Solution.solve()
