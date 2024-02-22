defmodule Day7 do
  def call do
    instructions = read_input("input.txt")
    order = determine_order(instructions)
    IO.puts("Order of completion: #{order}")
  end

  defp read_input(file_path) do
    File.read!(file_path)
    |> String.split("\n", trim: true)
  end

  defp determine_order(instructions) do
    deps = Enum.reduce(instructions, %{}, fn instruction, acc ->
      [_, step_before, step_after] = Regex.run(~r/Step (\w) must be finished before step (\w) can begin./, instruction)
      acc
      |> Map.update(step_after, [step_before], &[step_before | &1])
      |> Map.update(step_before, [], &(&1))
    end)

    steps = Map.keys(deps) |> Enum.sort()
    complete_order(steps, deps, "")
  end

  defp complete_order([], _deps, order), do: order

  defp complete_order(steps, deps, order) do
    next_step = Enum.find(steps, fn step ->
      deps[step] |> Enum.all?(fn dep -> not Enum.member?(steps, dep) end)
    end)

    updated_steps = steps -- [next_step]
    updated_order = order <> next_step

    complete_order(updated_steps, deps, updated_order)
  end
end

Day7.call()
