defmodule Day25 do
  @subject_number 7
  @modulus 20201227

  def read_input do
    {card_public_key, door_public_key} =
      File.read!("input.txt")
      |> String.trim()
      |> String.split("\n")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()

    {card_public_key, door_public_key}
  end

  defp transform(subject_number, loop_size) do
    Enum.reduce(1..loop_size, 1, fn _i, value ->
      rem(value * subject_number, @modulus)
    end)
  end

  defp find_loop_size(target_public_key) do
    find_loop_size(1, 0, target_public_key)
  end

  defp find_loop_size(value, loop_size, target_public_key) when value == target_public_key do
    loop_size
  end

  defp find_loop_size(value, loop_size, target_public_key) do
    new_value = rem(value * @subject_number, @modulus)
    find_loop_size(new_value, loop_size + 1, target_public_key)
  end

  def call do
    {card_public_key, door_public_key} = read_input()

    card_loop_size = find_loop_size(card_public_key)
    # Alternatively, find door_loop_size and use card_public_key
    encryption_key = transform(door_public_key, card_loop_size)

    IO.puts("Encryption key: #{encryption_key}")
  end
end

Day25.call()

