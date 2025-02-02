
defmodule Day16 do
  def solve(filename) do
    initial_state = read_initial_state(filename)
    data = generate_data(initial_state, 35_651_584)
    checksum = calculate_checksum(data)
    IO.puts("Checksum: #{checksum}")
  end

  def read_initial_state(filename) do
    case File.read(filename) do
      {:ok, content} -> String.trim(content)
      {:error, _} -> raise "Failed to read initial state"
    end
  end

  def generate_data(initial_state, length) do
    generate_data_recursive(initial_state, length)
  end

  defp generate_data_recursive(data, length) when byte_size(data) >= length, do: data |> String.slice(0, length)
  defp generate_data_recursive(data, length) do
    reversed = data
    |> String.to_charlist()
    |> Enum.reverse()
    |> Enum.map(fn
      ?0 -> ?1
      ?1 -> ?0
    end)
    |> List.to_string()
    generate_data_recursive(data <> "0" <> reversed, length)
  end


  def calculate_checksum(data) do
     calculate_checksum_recursive(data)
  end

  defp calculate_checksum_recursive(data) when rem(byte_size(data), 2) != 0, do: data
  defp calculate_checksum_recursive(data) do
     checksum =  data
     |> String.to_charlist()
      |> Enum.chunk_every(2)
        |> Enum.map(fn [a,b] ->
         if a == b do
          ?1
        else
         ?0
       end
      end)
     |> List.to_string()
     calculate_checksum_recursive(checksum)
  end
end

Day16.solve("input.txt")
