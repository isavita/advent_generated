
defmodule ChronalCalibration do
  def call do
    input = File.read!("input.txt")
    |> String.trim()
    |> String.split()
    |> Enum.map(&String.to_integer/1)

    part_one = Enum.sum(input)
    part_two = find_first_duplicate_frequency(input)

    {part_one, part_two}
  end

  defp find_first_duplicate_frequency(changes) do
    Stream.cycle(changes)
    |> Enum.reduce({0, MapSet.new()}, fn change, {current_freq, seen} ->
      new_freq = current_freq + change
      if MapSet.member?(seen, new_freq) do
        throw {:halt, new_freq}
      else
        {new_freq, MapSet.put(seen, new_freq)}
      end
    end)
    catch
      :throw, {:halt, freq} -> freq
  end
end
