defmodule MedicineMolecule do
  def solve do
    # Read input from file
    input = read_input()

    # Parse the replacements and medicine molecule from the input
    {replacements, medicine_molecule} = parse_input(input)

    # Generate all possible molecules after one replacement
    distinct_molecules = generate_distinct_molecules(replacements, medicine_molecule)

    # Count the number of distinct molecules
    num_distinct_molecules = length(distinct_molecules)

    # Print the result
    IO.puts("Number of distinct molecules after one replacement: #{num_distinct_molecules}")
  end

  defp read_input do
    File.read!("input.txt")
    |> String.trim()
  end

  defp parse_input(input) do
    [replacements_input, medicine_molecule] = String.split(input, "\n\n", parts: 2)

    replacements =
      String.split(replacements_input, "\n")
      |> Enum.map(fn line ->
        [from, to] = String.split(line, " => ")
        {from, to}
      end)

    {replacements, medicine_molecule}
  end

  defp generate_distinct_molecules(replacements, molecule) do
    replacements
    |> Enum.flat_map(fn {from, to} ->
      replace_at_all_positions(molecule, from, to)
    end)
    |> Enum.uniq()
  end

  defp replace_at_all_positions(molecule, from, to) do
    molecule_chars = String.graphemes(molecule)
    from_length = String.length(from)

    0..(String.length(molecule) - from_length)
    |> Enum.filter(fn i ->
      Enum.slice(molecule_chars, i, from_length) == String.graphemes(from)
    end)
    |> Enum.map(fn i ->
      {left, right} = String.split_at(molecule, i)
      left <> to <> String.slice(right, from_length..-1)
    end)
  end
end

MedicineMolecule.solve()
