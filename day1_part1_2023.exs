
defmodule Trebuchet do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&extract_and_sum/1)
    |> Enum.sum()
  end

  defp extract_and_sum(line) do
    digits = String.graphemes(line) |> Enum.filter(&String.match?(&1, ~r/\d/))
    case digits do
      [first | _] = list -> 
        last = List.last(list)
        String.to_integer(first <> last)
      _ -> 0
    end
  end
end
