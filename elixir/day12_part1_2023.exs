
defmodule Solver do
  def parse_input(input) do
    Enum.map(input, fn line ->
      [springs, groups_str] = String.split(line, " ")
      groups =
        groups_str
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)

      {springs, groups}
    end)
  end

  def count_arrangements({springs, groups}) do
    count_arrangements_recursive(springs, groups, 0, 0, 0, %{})
  end

  defp count_arrangements_recursive(springs, groups, i_springs, i_group, i_contiguous_damaged, cache) do
    key = {i_springs, i_group, i_contiguous_damaged}

    case Map.fetch(cache, key) do
      {:ok, val} ->
        val

      :error ->
        res =
          cond do
            i_springs == String.length(springs) ->
              cond do
                i_group == length(groups) and i_contiguous_damaged == 0 ->
                  1

                i_group == length(groups) - 1 and i_contiguous_damaged == Enum.at(groups, i_group) ->
                  1

                true ->
                  0
              end

            true ->
              char = String.at(springs, i_springs)

              case char do
                "." ->
                  if i_contiguous_damaged == 0 do
                    count_arrangements_recursive(
                      springs,
                      groups,
                      i_springs + 1,
                      i_group,
                      i_contiguous_damaged,
                      cache
                    )
                  else
                    if i_group < length(groups) and i_contiguous_damaged == Enum.at(groups, i_group) do
                      count_arrangements_recursive(springs, groups, i_springs + 1, i_group + 1, 0, cache)
                    else
                      0
                    end
                  end

                "#" ->
                  if i_group < length(groups) and
                       i_contiguous_damaged < Enum.at(groups, i_group) do
                    count_arrangements_recursive(
                      springs,
                      groups,
                      i_springs + 1,
                      i_group,
                      i_contiguous_damaged + 1,
                      cache
                    )
                  else
                    0
                  end

                "?" ->
                  case1 =
                    if i_contiguous_damaged == 0 do
                      count_arrangements_recursive(
                        springs,
                        groups,
                        i_springs + 1,
                        i_group,
                        i_contiguous_damaged,
                        cache
                      )
                    else
                      if i_group < length(groups) and i_contiguous_damaged == Enum.at(groups, i_group) do
                        count_arrangements_recursive(springs, groups, i_springs + 1, i_group + 1, 0, cache)
                      else
                        0
                      end
                    end

                  case2 =
                    if i_group < length(groups) and
                         i_contiguous_damaged < Enum.at(groups, i_group) do
                      count_arrangements_recursive(
                        springs,
                        groups,
                        i_springs + 1,
                        i_group,
                        i_contiguous_damaged + 1,
                        cache
                      )
                    else
                      0
                    end

                  case1 + case2
              end
          end

        Map.put(cache, key, res)
        |> (&count_arrangements_recursive(springs, groups, i_springs, i_group, i_contiguous_damaged, &1)).()
    end
  end

  def solve(input) do
    rows = parse_input(input)

    Enum.reduce(rows, 0, fn row, acc ->
      acc + count_arrangements(row)
    end)
  end

  def read_file(file_name) do
    {:ok, contents} = File.read(file_name)
    String.split(String.trim(contents), "\n")
  end
end

input = Solver.read_file("input.txt")
IO.puts(Solver.solve(input))
