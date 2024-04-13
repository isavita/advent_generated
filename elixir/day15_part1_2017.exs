defmodule GeneratorMatch do
  # Importing Bitwise module to use band/2 function
  import Bitwise

  def main do
    [genA_start, genB_start] = read_input("input.txt")

    genA_factor = 16807
    genB_factor = 48271
    modulus = 2147483647

    matches = Enum.reduce(1..40_000_000, {0, genA_start, genB_start}, fn _, {acc, genA, genB} ->
      new_genA = rem(genA * genA_factor, modulus)
      new_genB = rem(genB * genB_factor, modulus)
      if band(new_genA, 0xFFFF) == band(new_genB, 0xFFFF), do: {acc + 1, new_genA, new_genB}, else: {acc, new_genA, new_genB}
    end)
    |> elem(0)

    IO.puts(matches)
  end

  defp read_input(file_path) do
    file_path
    |> File.stream!()
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.to_integer/1)
  end
end

GeneratorMatch.main()