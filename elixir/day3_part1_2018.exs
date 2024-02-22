defmodule Day3 do
  def call do
    claims = read_input("input.txt")
    fabric_map = process_claims(claims)
    overlapping_area = count_overlapping(fabric_map)
    IO.puts(overlapping_area)
  end

  defp read_input(file_path) do
    File.stream!(file_path)
    |> Enum.map(&parse_claim/1)
  end

  defp parse_claim(claim) do
    # Use Regex.scan and properly extract the first match and its groups
    case Regex.scan(~r/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/, claim) do
      [[_, id_str, left_str, top_str, width_str, height_str]] ->
        {String.to_integer(id_str), 
         {String.to_integer(left_str), String.to_integer(top_str), 
          String.to_integer(width_str), String.to_integer(height_str)}}
    end
  end

  defp process_claims(claims) do
    Enum.reduce(claims, %{}, fn {_, {left, top, width, height}}, acc ->
      Enum.reduce(0..height-1, acc, fn dy, acc_inner ->
        Enum.reduce(0..width-1, acc_inner, fn dx, acc_inner_inner ->
          coordinate = {left + dx, top + dy}
          Map.update(acc_inner_inner, coordinate, 1, &(&1 + 1))
        end)
      end)
    end)
  end

  defp count_overlapping(fabric_map) do
    fabric_map
    |> Enum.count(fn {_key, value} -> value > 1 end)
  end
end

Day3.call()
