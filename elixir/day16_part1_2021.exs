defmodule PacketDecoder do
  def hex_to_bin(hex) do
    hex
    |> String.graphemes()
    |> Enum.map(&String.to_integer(&1, 16))
    |> Enum.map(&Integer.to_string(&1, 2) |> String.pad_leading(4, "0"))
    |> Enum.join()
  end

  def parse_packet(bin_str, idx) do
    version = String.slice(bin_str, idx, 3) |> binary_to_integer(2)
    type_id = String.slice(bin_str, idx + 3, 3) |> binary_to_integer(2)
    idx = idx + 6

    if type_id == 4 do
      parse_literal_value(bin_str, idx, version)
    else
      parse_operator(bin_str, idx, type_id, version)
    end
  end

  defp parse_literal_value(bin_str, idx, version) do
    {value, new_idx} = parse_literal_value_helper(bin_str, idx, "")
    {version, new_idx}
  end

  defp parse_literal_value_helper(bin_str, idx, value) do
    prefix = String.at(bin_str, idx)
    segment = String.slice(bin_str, idx + 1, 4)
    new_value = value <> segment
    new_idx = idx + 5

    if prefix == "0" do
      {String.to_integer(new_value, 2), new_idx}
    else
      parse_literal_value_helper(bin_str, new_idx, new_value)
    end
  end

  defp parse_operator(bin_str, idx, _type_id, version) do
    length_type_id = String.at(bin_str, idx) |> binary_to_integer(2)
    idx = idx + 1
    {num_sub_packets, sub_packet_length, idx} =
      if length_type_id == 0 do
        sub_packet_length = String.slice(bin_str, idx, 15) |> binary_to_integer(2)
        idx = idx + 15
        {0, sub_packet_length, idx}
      else
        num_sub_packets = String.slice(bin_str, idx, 11) |> binary_to_integer(2)
        idx = idx + 11
        {num_sub_packets, 0, idx}
      end

    version_sum = version
    while_loop(num_sub_packets, sub_packet_length, idx, length_type_id, version_sum, bin_str)
  end

  defp while_loop(0, 0, idx, _length_type_id, version_sum, _bin_str), do: {version_sum, idx}
  defp while_loop(num_sub_packets, sub_packet_length, idx, length_type_id, version_sum, bin_str) do
    {sub_version, new_idx} = parse_packet(bin_str, idx)
    version_sum = version_sum + sub_version
    if length_type_id == 0 do
      sub_packet_length = sub_packet_length - (new_idx - idx)
      while_loop(num_sub_packets, sub_packet_length, new_idx, length_type_id, version_sum, bin_str)
    else
      num_sub_packets = num_sub_packets - 1
      while_loop(num_sub_packets, sub_packet_length, new_idx, length_type_id, version_sum, bin_str)
    end
  end

  def binary_to_integer(binary, base) do
    binary |> String.to_integer(base)
  end

  def run() do
    {:ok, data} = File.read("input.txt")
    hex_str = String.trim(data)
    bin_str = hex_to_bin(hex_str)
    {version_sum, _} = parse_packet(bin_str, 0)
    IO.puts(version_sum)
  end
end

PacketDecoder.run()