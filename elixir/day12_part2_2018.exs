
defmodule Day21 do
  def solve do
    {initial_state, rules} = read_input("input.txt")
    state =
      initial_state
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _} -> char == "#" end)
      |> Enum.map(fn {_, index} -> {index, "#"} end)
      |> Map.new()

    solve_generations(state, rules, "", 0, 0, 50_000_000_000)
  end

  def read_input(filename) do
    File.read!(filename)
    |> String.split("\n", trim: true)
    |> Enum.reduce({"", %{}}, fn line, {initial_state, rules} ->
      cond do
        String.contains?(line, "initial state") ->
          {String.split(line, ": ", parts: 2) |> Enum.at(1), rules}

        String.contains?(line, "=>") ->
          [pattern, result] = String.split(line, " => ", parts: 2)
          {initial_state, Map.put(rules, pattern, String.first(result))}

        true ->
          {initial_state, rules}
      end
    end)
  end

  def solve_generations(state, rules, previous_pattern, previous_sum, generation, target_generations) do
    new_state =
      state
      |> Map.keys()
      |> min_max_keys()
      |> then(fn {min_pot, max_pot} ->
        for i <- (min_pot - 2)..(max_pot + 2), reduce: %{} do
          acc ->
            pattern = get_pattern(state, i)
            if Map.get(rules, pattern) == "#" do
              Map.put(acc, i, "#")
            else
              acc
            end
        end
      end)

    {current_pattern, current_sum} = state_pattern(new_state)

    cond do
      current_pattern == previous_pattern ->
        offset = current_sum - previous_sum
        remaining_generations = target_generations - generation - 1
        final_sum = current_sum + offset * remaining_generations
        IO.puts(final_sum)

      generation >= target_generations - 1 ->
        IO.puts(current_sum)

      true ->
        solve_generations(new_state, rules, current_pattern, current_sum, generation + 1, target_generations)
    end
  end

  def min_max_keys(keys) do
    keys = Enum.sort(keys)
    {List.first(keys), List.last(keys)}
  end

  def get_pattern(state, center_pot) do
    for pot <- (center_pot - 2)..(center_pot + 2), reduce: "" do
      acc ->
        acc <> if Map.get(state, pot) == "#", do: "#", else: "."
    end
  end

  def state_pattern(state) do
    keys = Map.keys(state)
    if Enum.empty?(keys) do
      {"", 0}
    else
      {min_key, max_key} = min_max_keys(keys)
      {pattern, sum} =
        Enum.reduce(min_key..max_key, {"", 0}, fn i, {pattern_acc, sum_acc} ->
          if Map.get(state, i) == "#" do
            {pattern_acc <> "#", sum_acc + i}
          else
            {pattern_acc <> ".", sum_acc}
          end
        end)
        {pattern, sum}
    end
  end
end

Day21.solve()
