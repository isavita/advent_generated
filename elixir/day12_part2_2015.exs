defmodule JsonSum do
  def main do
    input = File.read!("input.txt") |> to_string()
    part1 = sum_numbers_part1(input)
    part2 = sum_numbers_part2(input)
    IO.puts("Part 1 Sum: #{part1}")
    IO.puts("Part 2 Sum: #{part2}")
  end

  def sum_numbers_part1(json) do
    ~r/-?\d+/
    |> Regex.scan(json)
    |> List.flatten()
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end

  def sum_numbers_part2(json) do
    json
    |> parse_json()
    |> sum_numbers_recursive()
  end

  def sum_numbers_recursive(val) when is_integer(val), do: val
  def sum_numbers_recursive(list) when is_list(list) do
    Enum.reduce(list, 0, fn v, acc -> acc + sum_numbers_recursive(v) end)
  end
  def sum_numbers_recursive(map) when is_map(map) do
    if Enum.any?(map, fn {_k, v} -> v == "red" end) do
      0
    else
      Enum.reduce(map, 0, fn {_k, v}, acc -> acc + sum_numbers_recursive(v) end)
    end
  end
  def sum_numbers_recursive(_), do: 0

  def parse_json(s) when is_binary(s) do
    s = String.trim(s)
    cond do
      String.starts_with?(s, "[") -> parse_array(s)
      String.starts_with?(s, "{") -> parse_object(s)
      Regex.match?(~r/^-?\d+$/, s) -> String.to_integer(s)
      true -> parse_string(s)
    end
  end

  def parse_array(s) do
    inner_len = String.length(s) - 2
    inner = String.slice(s, 1, inner_len) |> String.trim()
    if inner == "" do
      []
    else
      inner
      |> split_top_level()
      |> Enum.map(&parse_json/1)
    end
  end

  def parse_object(s) do
    inner_len = String.length(s) - 2
    inner = String.slice(s, 1, inner_len) |> String.trim()
    if inner == "" do
      %{}
    else
      inner
      |> split_top_level()
      |> Enum.reduce(%{}, fn pair_str, acc ->
        case String.split(pair_str, ":", parts: 2) do
          [key_str, value_str] ->
            key = key_str |> String.trim() |> unquote_key()
            value = parse_json(value_str)
            Map.put(acc, key, value)
          _ -> acc
        end
      end)
    end
  end

  def unquote_key(str) do
    s = String.trim(str)
    if String.starts_with?(s, "\"") and String.ends_with?(s, "\"") do
      String.slice(s, 1, String.length(s) - 2)
    else
      s
    end
  end

  def parse_string(s) do
    if String.starts_with?(s, "\"") and String.ends_with?(s, "\"") and String.length(s) >= 2 do
      String.slice(s, 1, String.length(s) - 2)
    else
      s
    end
  end

  def split_top_level(s) do
    chars = String.graphemes(s)
    {parts, cur, _depth} =
      Enum.reduce(chars, {[], "", 0}, fn ch, {parts, cur, depth} ->
        case ch do
          "[" -> {parts, cur <> ch, depth + 1}
          "]" -> {parts, cur <> ch, depth - 1}
          "{" -> {parts, cur <> ch, depth + 1}
          "}" -> {parts, cur <> ch, depth - 1}
          "," when depth == 0 -> {parts ++ [cur], "", depth}
          _ -> {parts, cur <> ch, depth}
        end
      end)

    final = parts ++ [cur]
    Enum.map(final, &String.trim/1)
  end
end

JsonSum.main()