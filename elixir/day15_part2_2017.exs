defmodule GeneratorMatch do
  def run do
    [gen_a_start, gen_b_start] = File.read!("input.txt") |> String.split() |> Enum.map(&String.to_integer/1)

    gen_a_factor = 16807
    gen_b_factor = 48271
    modulus = 2147483647

    {matches, _, _} = 
      Enum.reduce(1..5000000, {0, gen_a_start, gen_b_start}, fn _, {matches, gen_a, gen_b} ->
        {new_gen_a, new_gen_b} = next_values(gen_a, gen_b, gen_a_factor, gen_b_factor, modulus)
        if values_match?(new_gen_a, new_gen_b), do: {matches + 1, new_gen_a, new_gen_b}, else: {matches, new_gen_a, new_gen_b}
      end)

    IO.puts(matches)
  end

  defp next_values(gen_a, gen_b, gen_a_factor, gen_b_factor, modulus) do
    new_gen_a = next_value(gen_a, gen_a_factor, modulus, 4)
    new_gen_b = next_value(gen_b, gen_b_factor, modulus, 8)
    {new_gen_a, new_gen_b}
  end

  defp next_value(value, factor, modulus, multiple_of) do
    new_value = rem(value * factor, modulus)
    if rem(new_value, multiple_of) == 0, do: new_value, else: next_value(new_value, factor, modulus, multiple_of)
  end

  defp values_match?(gen_a, gen_b) do
    rem(gen_a, 65536) == rem(gen_b, 65536)
  end
end

GeneratorMatch.run()