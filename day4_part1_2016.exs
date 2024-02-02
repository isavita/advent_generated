
defmodule Security do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_room/1)
    |> Enum.filter(&valid_room?/1)
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  defp parse_room(room) do
    [encrypted_name, sector_id, checksum] =
      Regex.run(~r/([a-z-]+)-(\d+)\[([a-z]+)\]/, room, capture: :all_but_first)
    {String.replace(encrypted_name, "-", ""), String.to_integer(sector_id), checksum}
  end

  defp valid_room?({name, _, checksum}) do
    name
    |> frequency()
    |> Enum.sort_by(&{-elem(&1, 1), elem(&1, 0)})
    |> Enum.map(&elem(&1, 0))
    |> Enum.take(5)
    |> Enum.join("")
    |> String.equivalent?(checksum)
  end

  defp frequency(name) do
    name
    |> String.graphemes()
    |> Enum.reduce(%{}, fn g, acc ->
      Map.update(acc, g, 1, &(&1 + 1))
    end)
    |> Map.to_list()
  end
end
