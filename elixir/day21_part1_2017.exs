
defmodule FractalArt do
  @initial_pattern ~w(.#./..#/###) |> Enum.map(&String.to_charlist/1)

  def parse_rules(rules_string) do
    String.split(rules_string, "\n", trim: true)
    |> Enum.map(fn rule ->
      [input, output] = String.split(rule, " => ")
      {String.split(input, "/", trim: true) |> Enum.map(&String.to_charlist/1), String.split(output, "/", trim: true) |> Enum.map(&String.to_charlist/1)}
    end)
  end

  def rotate(pattern) do
    size = length(pattern)
    Enum.map(0..(size - 1), fn i ->
      Enum.map(0..(size - 1), fn j ->
        pattern[size - 1 - j][i]
      end)
    end)
  end

  def flip_horizontal(pattern) do
    Enum.map(pattern, fn row -> Enum.reverse(row) end)
  end

  def pattern_to_string(pattern) do
    pattern |> Enum.map(&List.to_string/1) |> Enum.join("/")
  end

  def find_rule(pattern, rules) do
    variants = generate_variants(pattern)
    Enum.find_value(variants, nil, fn variant ->
      pattern_string = pattern_to_string(variant)
      case Enum.find(rules, fn {input, _} -> pattern_to_string(input) == pattern_string end) do
        {_, output} -> output
        nil -> nil
      end
    end)
  end

  def generate_variants(pattern) do
    [
      pattern,
      rotate(pattern),
      rotate(rotate(pattern)),
      rotate(rotate(rotate(pattern))),
      flip_horizontal(pattern),
      rotate(flip_horizontal(pattern)),
      rotate(rotate(flip_horizontal(pattern))),
      rotate(rotate(rotate(flip_horizontal(pattern))))
    ]
  end

  def enhance(pattern, rules) do
    size = length(pattern)
    if rem(size, 2) == 0 do
      enhance_grid(pattern, rules, 2)
    else
      enhance_grid(pattern, rules, 3)
    end
  end

  def enhance_grid(pattern, rules, block_size) do
    size = length(pattern)
    num_blocks = div(size, block_size)
    new_block_size = block_size + 1

    new_pattern =
      Enum.flat_map(0..(num_blocks - 1), fn i ->
        Enum.map(0..(num_blocks - 1), fn j ->
          block = extract_block(pattern, i, j, block_size)
          find_rule(block, rules)
        end)
        |> transpose_blocks(new_block_size)
      end)

    new_pattern
  end

  def extract_block(pattern, row_index, col_index, block_size) do
    start_row = row_index * block_size
    start_col = col_index * block_size

    Enum.map(start_row..(start_row + block_size - 1), fn i ->
      Enum.slice(Enum.at(pattern, i), start_col..(start_col + block_size - 1))
    end)
  end

  def transpose_blocks(blocks, block_size) do
    Enum.zip(blocks)
    |> Enum.map(fn block_tuple ->
      Enum.reduce(block_tuple, [], fn block, acc ->
        Enum.zip(block)
        |> Enum.map(&Tuple.to_list/1)
        |> Enum.with_index()
        |> Enum.map(fn {row, index} -> List.insert_at(acc, index, row) end)
        |> List.flatten()
      end)
      |> Enum.chunk_every(block_size)
    end)
    |> List.flatten()
  end

  def count_on_pixels(pattern) do
    Enum.reduce(pattern, 0, fn row, acc ->
      acc + Enum.count(row, fn pixel -> pixel == ?# end)
    end)
  end

  def main(input_file, iterations) do
    rules_string = File.read!(input_file)
    rules = parse_rules(rules_string)
    final_pattern = iterate(@initial_pattern, rules, iterations)
    count_on_pixels(final_pattern)
  end

  def iterate(pattern, rules, 0), do: pattern

  def iterate(pattern, rules, iterations) do
    new_pattern = enhance(pattern, rules)
    iterate(new_pattern, rules, iterations - 1)
  end
end

# Read the input file name and number of iterations from the command line.
input_file = System.argv() |> Enum.at(0)
iterations = System.argv() |> Enum.at(1) |> String.to_integer()

# Run the simulation and print the result.
result = FractalArt.main(input_file, iterations)
IO.puts(result)
