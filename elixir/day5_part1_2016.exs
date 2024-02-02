
defmodule SecurityDoor do
  def call do
    door_id = File.read!("input.txt") |> String.trim()
    Stream.iterate(0, &(&1 + 1))
    |> Stream.map(&hash(door_id, &1))
    |> Stream.filter(&String.starts_with?(&1, "00000"))
    |> Stream.map(&String.at(&1, 5))
    |> Enum.take(8)
    |> Enum.join()
  end

  defp hash(door_id, index) do
    :crypto.hash(:md5, door_id <> Integer.to_string(index))
    |> Base.encode16(case: :lower)
  end
end
