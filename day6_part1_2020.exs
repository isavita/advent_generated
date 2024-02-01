
defmodule CustomCustoms do
  def call do
    input = File.read!("input.txt")
    |> String.split("\n\n", trim: true)

    input
    |> Enum.map(&String.split(&1, "\n", trim: true))
    |> Enum.map(&Enum.join(&1))
    |> Enum.map(&String.graphemes/1)
    |> Enum.map(&Enum.uniq(&1))
    |> Enum.map(&Enum.count(&1))
    |> Enum.sum()
  end
end
