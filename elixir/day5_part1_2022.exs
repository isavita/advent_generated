
defmodule Main do
  def main do
    [diagram, moves] =
      File.read!("input.txt")
      |> String.split("\n\n", parts: 2)

    stack_lines = String.split(diagram, "\n", trim: true)

    num_stacks =
      stack_lines
      |> List.first()
      |> String.length()
      |> Kernel.+(1)
      |> Kernel./(4)
      |> trunc()

    init_stacks =
      for _ <- 1..num_stacks, do: []

    stacks =
      Enum.reduce(stack_lines, init_stacks, fn line, acc ->
        Enum.reduce(0..(num_stacks - 1), acc, fn idx, acc2 ->
          char = String.at(line, 1 + idx * 4)

          if char && char =~ ~r/[A-Z]/ do
            List.update_at(acc2, idx, &[char | &1])
          else
            acc2
          end
        end)
      end)
      |> Enum.map(&Enum.reverse/1)

    final_stacks =
      moves
      |> String.split("\n", trim: true)
      |> Enum.reduce(stacks, fn line, acc ->
        case Regex.run(~r/move (\d+) from (\d+) to (\d+)/, line) do
          [_full, n, from, to] ->
            {n, from, to} = {String.to_integer(n), String.to_integer(from) - 1, String.to_integer(to) - 1}
            Enum.reduce(1..n, acc, fn _, a ->
              [crate | rest] = Enum.at(a, from)
              a
              |> List.update_at(from, fn _ -> rest end)
              |> List.update_at(to, fn dst -> [crate | dst] end)
            end)

          _ -> acc
        end
      end)

    final_stacks
    |> Enum.map(fn [h | _] -> h end)
    |> Enum.join()
    |> IO.puts()
  end
end

Main.main()
