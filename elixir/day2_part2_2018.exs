defmodule Day1 do
  def call do
    box_ids = read_input("input.txt")
    case find_common_letters(box_ids) do
      nil -> IO.puts("No matching IDs found")
      common_letters -> IO.puts(common_letters)
    end
  end

  defp read_input(file_path) do
    File.read!(file_path)
    |> String.split("\n", trim: true)
  end

  defp find_common_letters(box_ids) do
    with_pairs = for id1 <- box_ids, id2 <- box_ids, id1 != id2, do: {id1, id2}
    Enum.reduce_while(with_pairs, nil, fn {id1, id2}, _acc ->
      case compare_ids(id1, id2) do
        {1, common} -> {:halt, common}
        _ -> {:cont, nil}
      end
    end)
  end

  defp compare_ids(id1, id2) do
    {diff, common} = Enum.reduce(Enum.zip(String.graphemes(id1), String.graphemes(id2)), {0, []}, fn
      {char1, char2}, {diff, acc} when char1 == char2 -> {diff, [char1 | acc]}
      _, {diff, acc} -> {diff + 1, acc}
    end)

    {diff, Enum.reverse(common) |> Enum.join("")}
  end
end

Day1.call()
