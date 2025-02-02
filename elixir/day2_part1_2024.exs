
defmodule Solution do
  def main do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.count(&is_safe_report/1)
    |> IO.puts()
  end

  defp is_safe_report(line) do
    line
    |> String.split(" ")
    |> Enum.map(&String.to_integer/1)
    |> check_levels()
  end

  defp check_levels(levels) when length(levels) < 2, do: false
  defp check_levels(levels) do
    first_diff = hd(tl(levels)) - hd(levels)
    if first_diff == 0 do
      false
    else
       is_increasing = first_diff > 0
       levels
       |> Enum.chunk_every(2, 1, :discard)
       |> Enum.all?(fn [a, b] ->
         diff = b - a
           if diff == 0 do
             false
           else
             abs_diff = abs(diff)
             if (is_increasing and diff <= 0) or (not is_increasing and diff >= 0) do
               false
             else
               abs_diff >= 1 and abs_diff <= 3
             end
           end
         end)
     end
   end
 end

Solution.main()
