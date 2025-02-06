
defmodule Day20 do
  def parse_input(input) do
    Enum.reduce(String.split(input, "\n", trim: true), {%{}, %{}}, fn line, {modules, adj} ->
      [name_part, dest_part] = String.split(line, " -> ")
      dests = String.split(dest_part, ", ", trim: true)

      case String.at(name_part, 0) do
        "%" ->
          {name, type} = {String.slice(name_part, 1..-1), :flip_flop}
          {Map.put(modules, name, {type, :off}), Map.put(adj, name, dests)}

        "&" ->
          {name, type} = {String.slice(name_part, 1..-1), :conjunction}
          {Map.put(modules, name, {type, %{}}), Map.put(adj, name, dests)}

        _ -> # broadcaster
          {name, type} = {name_part, :broadcaster}
          {Map.put(modules, name, {type, nil}), Map.put(adj, name, dests)}
      end
    end)
    |> then(fn {modules, adj} ->
      # Initialize conjunction module memory
      conjunctions =
        for {name, {type, _}} <- modules, type == :conjunction, do: name

      inputs =
        for {source, dests} <- adj, dest <- dests, Enum.member?(conjunctions, dest),
            do: {dest, source}

      Enum.reduce(inputs, modules, fn {conj_name, input_name}, acc ->
        Map.update!(acc, conj_name, fn {type, memory} ->
          {type, Map.put(memory, input_name, :low)}
        end)
      end)
      |> then(fn updated_modules -> {updated_modules, adj} end)
    end)
  end

  def process_pulse(modules, adj, {sender, receiver, pulse}) do
    if Map.has_key?(modules, receiver) do
      {type, state} = modules[receiver]

      case type do
        :broadcaster ->
          {modules, Enum.map(adj[receiver], fn dest -> {receiver, dest, pulse} end), pulse}

        :flip_flop ->
          if pulse == :high do
            {modules, [], pulse}
          else
            new_state = if state == :off, do: :on, else: :off
            new_pulse = if state == :off, do: :high, else: :low
            updated_modules = Map.put(modules, receiver, {:flip_flop, new_state})

            {updated_modules,
             Enum.map(adj[receiver], fn dest -> {receiver, dest, new_pulse} end), pulse}
          end

        :conjunction ->
          updated_memory = Map.put(state, sender, pulse)
          updated_modules = Map.put(modules, receiver, {:conjunction, updated_memory})
          new_pulse =
            if Enum.all?(Map.values(updated_memory), &(&1 == :high)),
              do: :low,
              else: :high
            
          {updated_modules,
            Enum.map(adj[receiver], fn dest -> {receiver, dest, new_pulse} end), pulse}
      end
    else
        {modules, [], pulse}
    end
  end

  def run(filename \\ "input.txt") do
    {modules, adj} =
      File.read!(filename)
      |> parse_input()

    initial_state = {modules, %{low: 0, high: 0}, []}

    {_, counts, _} =
      Enum.reduce(1..1000, initial_state, fn _, {modules, counts, _} ->
        queue = [({"button", "broadcaster", :low})]
        process_queue(modules, adj, queue, counts)
      end)

    counts.low * counts.high
  end

  def process_queue(modules, adj, queue, counts) do
      case queue do
        [] -> {modules, counts, []}
        [{sender, receiver, pulse} | rest_queue] ->

          new_counts = %{
            low: counts.low + (if pulse == :low, do: 1, else: 0),
            high: counts.high + (if pulse == :high, do: 1, else: 0)
          }

          {updated_modules, new_pulses, _} = process_pulse(modules, adj, {sender, receiver, pulse})

          process_queue(updated_modules, adj, rest_queue ++ new_pulses, new_counts)
      end
  end
end

IO.puts(Day20.run())
