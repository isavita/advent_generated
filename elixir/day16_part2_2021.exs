
defmodule Day16 do
  def hex_to_bin(hex) do
    hex
    |> String.graphemes()
    |> Enum.map(&hex_to_bin_char/1)
    |> Enum.join()
  end

  defp hex_to_bin_char(hex_char) do
    hex_char
    |> String.to_integer(16)
    |> Integer.to_string(2)
    |> String.pad_leading(4, "0")
  end

  def parse_packet(bin_str, idx) do
    {version, idx} = parse_version(bin_str, idx)
    {type_id, idx} = parse_type_id(bin_str, idx)

    case type_id do
      4 -> parse_literal_value(bin_str, idx)
      _ -> parse_operator_packet(bin_str, idx, type_id)
    end
  end

  defp parse_version(bin_str, idx) do
    {version, idx} = parse_bits(bin_str, idx, 3)
    {version, idx}
  end

  defp parse_type_id(bin_str, idx) do
    {type_id, idx} = parse_bits(bin_str, idx, 3)
    {type_id, idx}
  end

  defp parse_literal_value(bin_str, idx) do
    {value, idx} = parse_literal_value_recursive(bin_str, idx, 0)
    {0, idx, value}
  end

  defp parse_literal_value_recursive(bin_str, idx, acc) do
    {prefix, idx} = parse_bit(bin_str, idx)
    {value, idx} = parse_bits(bin_str, idx, 4)
    new_acc = Bitwise.bsl(acc, 4) + value

    case prefix do
      1 -> parse_literal_value_recursive(bin_str, idx, new_acc)
      0 -> {new_acc, idx}
    end
  end

  defp parse_operator_packet(bin_str, idx, type_id) do
    {length_type_id, idx} = parse_bit(bin_str, idx)
    {idx, values} =
      case length_type_id do
        0 -> parse_subpackets_by_length(bin_str, idx)
        1 -> parse_subpackets_by_count(bin_str, idx)
      end

    {0, idx, calculate_result(type_id, values)}
  end

  defp parse_subpackets_by_length(bin_str, idx) do
    {subpacket_length, idx} = parse_bits(bin_str, idx, 15)
    parse_subpackets_by_length_recursive(bin_str, idx, subpacket_length, [])
  end

  defp parse_subpackets_by_length_recursive(bin_str, idx, subpacket_length, acc) do
    if subpacket_length <= 0 do
      {idx, Enum.reverse(acc)}
    else
      {_, new_idx, sub_value} = parse_packet(bin_str, idx)
      parse_subpackets_by_length_recursive(
        bin_str,
        new_idx,
        subpacket_length - (new_idx - idx),
        [sub_value | acc]
      )
    end
  end

  defp parse_subpackets_by_count(bin_str, idx) do
    {num_subpackets, idx} = parse_bits(bin_str, idx, 11)
    parse_subpackets_by_count_recursive(bin_str, idx, num_subpackets, [])
  end

  defp parse_subpackets_by_count_recursive(bin_str, idx, num_subpackets, acc) do
    if num_subpackets <= 0 do
      {idx, Enum.reverse(acc)}
    else
      {_, new_idx, sub_value} = parse_packet(bin_str, idx)
      parse_subpackets_by_count_recursive(
        bin_str,
        new_idx,
        num_subpackets - 1,
        [sub_value | acc]
      )
    end
  end

  defp calculate_result(type_id, values) do
    case type_id do
      0 -> Enum.sum(values)
      1 -> Enum.product(values)
      2 -> Enum.min(values)
      3 -> Enum.max(values)
      5 -> if Enum.at(values, 0) > Enum.at(values, 1), do: 1, else: 0
      6 -> if Enum.at(values, 0) < Enum.at(values, 1), do: 1, else: 0
      7 -> if Enum.at(values, 0) == Enum.at(values, 1), do: 1, else: 0
    end
  end

  defp parse_bits(bin_str, idx, count) do
    {value, rest} = String.split_at(bin_str, idx + count)
    {value |> String.slice(idx, count) |> String.to_integer(2), idx + count}
  end

  defp parse_bit(bin_str, idx) do
    {String.slice(bin_str, idx, 1) |> String.to_integer(), idx + 1}
  end

  def solve() do
    input = File.read!("input.txt") |> String.trim()
    bin_str = hex_to_bin(input)
    {_, _, result} = parse_packet(bin_str, 0)
    IO.puts(result)
  end
end

Day16.solve()
