
defmodule Solver do
  def solve(input) do
    {reverse_graph, starting_mols} = parse_input(input)

    product_to_reactant =
      for {react, products} <- reverse_graph, p <- products, reduce: %{} do
        acc -> if Map.has_key?(acc, p), do: raise("dup found"), else: Map.put(acc, p, react)
      end

    all_products = Map.keys(product_to_reactant)

    start = Enum.join(starting_mols, "")

    do_solve(start, product_to_reactant, all_products, 0, start)
  end

  defp do_solve("e", _, _, steps, _), do: steps

  defp do_solve(mol, product_to_reactant, all_products, steps, start) do
    case make_change(mol, product_to_reactant, all_products) do
      {true, new_mol, additional_steps} ->
        do_solve(new_mol, product_to_reactant, all_products, steps + additional_steps, start)

      {false, _, _} ->
        all_products = Enum.shuffle(all_products)
        do_solve(start, product_to_reactant, all_products, 0, start)
    end
  end

  defp make_change(mol, product_to_reactant, all_products) do
    Enum.reduce_while(all_products, {false, mol, 0}, fn prod, _acc ->
      count = String.split(mol, prod) |> length() |> Kernel.-(1)

      if count <= 0 do
        {:cont, {false, mol, 0}}
      else
        new_mol = String.replace(mol, prod, product_to_reactant[prod])
        {:halt, {true, new_mol, count}}
      end
    end)
  end

  defp parse_input(input) do
    [blocks_0, blocks_1] = String.split(input, "\n\n")
    starting_material = split_molecules(blocks_1)

    graph =
      for l <- String.split(blocks_0, "\n"), reduce: %{} do
        acc ->
          [left, right] = String.split(l, " => ")
          Map.update(acc, left, [right], &[right | &1])
      end

    {graph, starting_material}
  end

  defp split_molecules(input) do
    Regex.scan(~r/[A-Z][a-z]?/, input) |> List.flatten()
  end
end

input = File.read!("input.txt") |> String.trim()
Solver.solve(input) |> IO.puts()
