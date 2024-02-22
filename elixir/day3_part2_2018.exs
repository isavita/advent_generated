defmodule Day3 do
  def call do
    claims = read_input("input.txt")
    {_, intact_claims} = process_claims(claims)
    non_overlapping_id = find_non_overlapping_claim(intact_claims)
    IO.puts(non_overlapping_id)
  end

  defp read_input(file_path) do
    File.stream!(file_path)
    |> Enum.map(&parse_claim/1)
  end

  defp parse_claim(claim) do
    case Regex.scan(~r/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/, claim) do
      [[_, id_str, left_str, top_str, width_str, height_str]] ->
        {String.to_integer(id_str), 
         {String.to_integer(left_str), String.to_integer(top_str), 
          String.to_integer(width_str), String.to_integer(height_str)}}
    end
  end

  defp process_claims(claims) do
    Enum.reduce(claims, {%{}, MapSet.new(Enum.map(claims, fn {id, _} -> id end))}, fn {id, {left, top, width, height}}, {acc, intact_ids} ->
      Enum.reduce(0..height-1, {acc, intact_ids}, fn dy, {acc_inner, intact_ids_inner} ->
        Enum.reduce(0..width-1, {acc_inner, intact_ids_inner}, fn dx, {acc_inner_inner, intact_ids_inner_inner} ->
          coordinate = {left + dx, top + dy}
          case Map.get(acc_inner_inner, coordinate) do
            nil -> {Map.put(acc_inner_inner, coordinate, id), intact_ids_inner_inner}
            existing_id -> 
              # If this coordinate is already claimed, remove both this ID and the existing ID from the set of intact IDs
              {acc_inner_inner, MapSet.delete(MapSet.delete(intact_ids_inner_inner, id), existing_id)}
          end
        end)
      end)
    end)
  end

  defp find_non_overlapping_claim(intact_claims) do
    case MapSet.to_list(intact_claims) do
      [non_overlapping_id] -> non_overlapping_id
      _ -> "No unique non-overlapping claim found"
    end
  end
end

Day3.call()
