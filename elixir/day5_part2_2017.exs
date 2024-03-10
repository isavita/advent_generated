defmodule PasswordDecryptor do
  def solve(door_id) do
    Stream.iterate(0, &(&1 + 1))
    |> Stream.map(fn index -> {index, md5("#{door_id}#{index}")} end)
    |> Stream.filter(fn {_, hash} -> String.starts_with?(hash, "00000") end)
    |> Stream.map(fn {_, hash} -> {String.at(hash, 5), String.at(hash, 6)} end)
    |> Stream.filter(fn {pos, _} -> pos >= "0" and pos <= "7" end)
    |> Stream.uniq_by(fn {pos, _} -> pos end)
    |> Stream.take(8)
    |> Enum.reduce(List.duplicate("_", 8), fn {pos, char}, password ->
      List.replace_at(password, String.to_integer(pos), char)
    end)
    |> Enum.join()
  end

  defp md5(input) do
    :crypto.hash(:md5, input) |> Base.encode16(case: :lower)
  end
end

# Read input from file
door_id = File.read!("input.txt") |> String.trim()

# Solve the password
password = PasswordDecryptor.solve(door_id)

# Print the result
IO.puts("Password: #{password}")
