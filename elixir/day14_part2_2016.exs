
defmodule Day14 do
  require Logger
  @moduledoc """
  Advent of Code 2016, Day 14
  """

  @doc """
  Finds the index that produces the 64th key using the given salt and a stretched MD5 hash.
  """
  def main do
    salt = read_input()
    index_of_64th_key = find_keys(salt)
    IO.puts(index_of_64th_key)
  end

  defp read_input do
    {:ok, file} = File.read("input.txt")
    String.trim(file)
  end

  defp md5_hash(input_str) do
    :crypto.hash(:md5, input_str) |> Base.encode16(case: :lower)
  end

  defp stretched_md5_hash(input_str) do
    hash_result = md5_hash(input_str)

    Enum.reduce(1..2016, hash_result, fn _, acc ->
      md5_hash(acc)
    end)
  end

  defp find_keys(salt, key_index \\ 64) do
    find_keys(salt, key_index, 0, %{}, [])
  end

  defp find_keys(salt, key_index, i, potential_keys, confirmed_keys) do
    cond do
      length(confirmed_keys) >= key_index ->
        confirmed_keys
        |> Enum.sort()
        |> Enum.at(key_index - 1)

      true ->
        hash_result = stretched_md5_hash("#{salt}#{i}")

        {new_potential_keys, new_confirmed_keys} =
          Enum.reduce(Map.keys(potential_keys), {potential_keys, confirmed_keys}, fn char, {potential_keys_acc, confirmed_keys_acc} ->
            if String.contains?(hash_result, String.duplicate(char, 5)) do
              Enum.reduce(potential_keys_acc[char], {potential_keys_acc, confirmed_keys_acc}, fn potential_index, {potential_keys_inner, confirmed_keys_inner} ->
                if i - potential_index <= 1000 do
                  {potential_keys_inner, [potential_index | confirmed_keys_inner]}
                else
                  {potential_keys_inner, confirmed_keys_inner}
                end
              end)
              |> then(fn {potential_keys_inner, confirmed_keys_inner} ->
                {
                  Map.update(potential_keys_inner, char, [], fn _ -> [] end),
                  confirmed_keys_inner
                }
              end)
            else
              {potential_keys_acc, confirmed_keys_acc}
            end
          end)

        potential_keys_cleaned =
          Map.new(new_potential_keys, fn {char, indexes} ->
            {char, Enum.filter(indexes, fn index -> i - index < 1000 end)}
          end)

        new_char = find_triplet(hash_result)

        potential_keys_updated =
          case new_char do
            nil ->
              potential_keys_cleaned

            char ->
              Map.update(potential_keys_cleaned, char, [i], fn existing -> [i | existing] end)
          end

        find_keys(salt, key_index, i + 1, potential_keys_updated, new_confirmed_keys)
    end
  end

  defp find_triplet(hash_result) do
    String.codepoints(hash_result)
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.find_value(nil, fn [a, b, c] ->
      if a == b and b == c do
        a
      else
        nil
      end
    end)
  end
end

Day14.main()
