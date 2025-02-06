
defmodule Day16 do
  def dance(programs, moves, iterations \\ 1) do
    programs = to_charlist(programs)
    seen = %{}
    cycle_length = find_cycle(programs, moves, seen, 0)

    iterations_mod = rem(iterations, cycle_length)

    do_dance(programs, moves, iterations_mod)
    |> to_string()
  end

  defp find_cycle(programs, moves, seen, count) do
    if Map.has_key?(seen, programs) do
      count - seen[programs]
    else
      new_programs = perform_moves(programs, moves)
      find_cycle(new_programs, moves, Map.put(seen, programs, count), count + 1)
    end
  end

  defp do_dance(programs, moves, iterations) when iterations == 0, do: programs

  defp do_dance(programs, moves, iterations) do
    programs
    |> perform_moves(moves)
    |> do_dance(moves, iterations - 1)
  end

  defp perform_moves(programs, moves) do
    Enum.reduce(moves, programs, fn move, acc ->
      apply_move(acc, move)
    end)
  end

  defp apply_move(programs, "s" <> size) do
    size = String.to_integer(size)
    {tail, head} = Enum.split(programs, length(programs) - size)
    head ++ tail
  end

  defp apply_move(programs, "x" <> positions) do
    [a, b] =
      positions
      |> String.split("/")
      |> Enum.map(&String.to_integer/1)

    swap(programs, a, b)
  end

  defp apply_move(programs, "p" <> programs_str) do
    [a, b] =
      programs_str
      |> String.split("/")
      |> Enum.map(&String.to_charlist/1)
      |> Enum.map(&hd/1)

    a_index = Enum.find_index(programs, &(&1 == a))
    b_index = Enum.find_index(programs, &(&1 == b))

    swap(programs, a_index, b_index)
  end

  defp swap(list, a, b) do
    elem_a = Enum.at(list, a)
    elem_b = Enum.at(list, b)

    list
    |> List.replace_at(a, elem_b)
    |> List.replace_at(b, elem_a)
  end
end

input =
  File.read!("input.txt")
  |> String.trim()
  |> String.split(",")

initial_programs = "abcdefghijklmnop"

# Part 1
result1 = Day16.dance(initial_programs, input)
IO.puts("Part 1: #{result1}")

# Part 2
result2 = Day16.dance(initial_programs, input, 1_000_000_000)
IO.puts("Part 2: #{result2}")
