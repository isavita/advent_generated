
defmodule Diagnostic do
  def call do
    input = File.read!("input.txt") |> String.split("\n", trim: true)
    {gamma, epsilon} = calculate_rates(input)
    power_consumption = String.to_integer(gamma, 2) * String.to_integer(epsilon, 2)
    {oxygen, co2} = calculate_life_support(input)
    life_support = String.to_integer(oxygen, 2) * String.to_integer(co2, 2)
    {power_consumption, life_support}
  end

  defp calculate_rates(input) do
    length = String.length(Enum.at(input, 0))
    gamma = for i <- 0..(length - 1), into: "" do
      Enum.map(input, &String.at(&1, i))
      |> Enum.frequencies()
      |> Enum.max_by(fn {_bit, count} -> count end)
      |> elem(0)
    end
    epsilon = String.replace(gamma, ~r/[01]/, fn "0" -> "1"; "1" -> "0" end)
    {gamma, epsilon}
  end

  defp calculate_life_support(input) do
    oxygen = filter_rating(input, &most_common_or_equal/1)
    co2 = filter_rating(input, &least_common_or_equal/1)
    {oxygen, co2}
  end

  defp filter_rating(input, criteria_func, bit_index \\ 0)

  defp filter_rating([rating], _criteria_func, _bit_index), do: rating

  defp filter_rating(input, criteria_func, bit_index) do
    bit_criteria = input
    |> Enum.map(&String.at(&1, bit_index))
    |> criteria_func.()
    filtered = Enum.filter(input, fn number -> String.at(number, bit_index) == bit_criteria end)
    filter_rating(filtered, criteria_func, bit_index + 1)
  end

  defp most_common_or_equal(bits) do
    frequencies = Enum.frequencies(bits)
    case Map.get(frequencies, "1", 0) >= Map.get(frequencies, "0", 0) do
      true -> "1"
      false -> "0"
    end
  end

  defp least_common_or_equal(bits) do
    frequencies = Enum.frequencies(bits)
    case Map.get(frequencies, "0", 0) <= Map.get(frequencies, "1", 0) do
      true -> "0"
      false -> "1"
    end
  end
end
