defmodule OneTimePad do
  def solve do
    # Read input from file
    salt = read_input()

    # Generate the 64th one-time pad key
    {index, _key} = generate_key(salt, 0, 0)

    # Print the result
    IO.puts("Index that produces the 64th key: #{index}")
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
  end

  defp generate_key(salt, index, key_count) do
    cond do
      key_count == 64 ->
        {index - 1, key_count}

      true ->
        hash = md5("#{salt}#{index}")

        case is_key?(hash) do
          {true, char} ->
            if has_five_of_a_kind?(salt, index + 1, char) do
              generate_key(salt, index + 1, key_count + 1)
            else
              generate_key(salt, index + 1, key_count)
            end

          false ->
            generate_key(salt, index + 1, key_count)
        end
    end
  end

  defp is_key?(hash) do
    case Regex.run(~r/(.)\1{2}/, hash) do
      [_, char] -> {true, char}
      nil -> false
    end
  end

  defp has_five_of_a_kind?(salt, start_index, char) do
    Enum.any?(start_index..(start_index + 999), fn index ->
      hash = md5("#{salt}#{index}")
      String.contains?(hash, String.duplicate(char, 5))
    end)
  end

  defp md5(input) do
    :crypto.hash(:md5, input)
    |> Base.encode16(case: :lower)
  end
end

OneTimePad.solve()
