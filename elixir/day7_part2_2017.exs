defmodule SSLSupporter do
  def solve do
    # Read input from file
    input = read_input()

    # Count the number of IPs that support SSL
    ssl_count = Enum.count(input, &supports_ssl?/1)

    # Print the result
    IO.puts("Number of IPs that support SSL: #{ssl_count}")
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
  end

  defp supports_ssl?(ip) do
    supernet_sequences = get_supernet_sequences(ip)
    hypernet_sequences = get_hypernet_sequences(ip)

    Enum.any?(supernet_sequences, fn seq ->
      abas = get_abas(seq)
      Enum.any?(abas, fn aba -> has_corresponding_bab?(aba, hypernet_sequences) end)
    end)
  end

  defp get_supernet_sequences(ip) do
    Regex.split(~r/\[.*?\]/, ip)
  end

  defp get_hypernet_sequences(ip) do
    Regex.scan(~r/\[(.*?)\]/, ip)
    |> List.flatten()
  end

  defp get_abas(sequence) do
    for i <- 0..(String.length(sequence) - 3),
        a = String.at(sequence, i),
        b = String.at(sequence, i + 1),
        c = String.at(sequence, i + 2),
        a == c and a != b,
        do: a <> b <> c
  end

  defp has_corresponding_bab?(aba, hypernet_sequences) do
    [a, b, _] = String.graphemes(aba)
    bab = b <> a <> b
    Enum.any?(hypernet_sequences, &String.contains?(&1, bab))
  end
end

SSLSupporter.solve()
