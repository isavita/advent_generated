
defmodule Day20 do
  def parse_input(filename) do
    filename
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, &parse_module/2)
    |> initialize_watches()
  end

  defp parse_module(line, modules) do
    cond do
      String.contains?(line, "broadcaster") ->
        [name, destinations] = String.split(line, " -> ")
        Map.put(modules, name, %{
          type: :broadcaster, 
          destinations: String.split(destinations, ", "),
          state: nil
        })

      String.starts_with?(line, "%") ->
        [name, destinations] = String.split(line, " -> ")
        name = String.slice(name, 1..-1)
        Map.put(modules, name, %{
          type: :flip_flop, 
          destinations: String.split(destinations, ", "),
          state: false
        })

      String.starts_with?(line, "&") ->
        [name, destinations] = String.split(line, " -> ")
        name = String.slice(name, 1..-1)
        Map.put(modules, name, %{
          type: :conjunction, 
          destinations: String.split(destinations, ", "),
          inputs: %{}
        })
    end
  end

  defp initialize_watches(modules) do
    modules
    |> Enum.reduce(modules, fn {name, module}, acc ->
      module.destinations
      |> Enum.filter(&Map.has_key?(acc, &1))
      |> Enum.reduce(acc, fn dest, inner_acc ->
        update_in(inner_acc[dest][:inputs], fn 
          nil -> %{name => false}
          inputs -> Map.put(inputs, name, false)
        end)
      end)
    end)
  end

  def solve(filename) do
    modules = parse_input(filename)
    find_cycle_length(modules)
  end

  defp find_cycle_length(modules) do
    rx_input = find_rx_input(modules)
    
    rx_input
    |> Map.keys()
    |> Enum.map(fn input -> find_input_cycle(modules, input) end)
    |> Enum.reduce(&lcm/2)
  end

  defp find_rx_input(modules) do
    modules
    |> Enum.find(fn {_, module} -> 
      Enum.member?(module.destinations, "rx")
    end)
    |> elem(1)
    |> Map.get(:inputs)
  end

  defp find_input_cycle(modules, start_input) do
    Stream.iterate(1, &(&1 + 1))
    |> Enum.reduce_while({modules, start_input}, fn press, {curr_modules, input} ->
      {new_modules, pulses} = press_button(curr_modules)
      
      high_pulse_to_input = Enum.any?(pulses, fn 
        {^input, _, true} -> true 
        _ -> false
      end)
      
      if high_pulse_to_input do
        {:halt, press}
      else
        {:cont, {new_modules, input}}
      end
    end)
  end

  defp press_button(modules) do
    do_press_button(modules, [{nil, "broadcaster", false}], [], [])
  end

  defp do_press_button(modules, [], processed_pulses, acc_pulses) do
    {modules, Enum.reverse(processed_pulses)}
  end

  defp do_press_button(modules, [{from, name, pulse} | queue], processed_pulses, acc_pulses) do
    case Map.get(modules, name) do
      nil -> 
        do_press_button(modules, queue, [{from, name, pulse} | processed_pulses], acc_pulses)
      
      %{type: :broadcaster} = module ->
        new_pulses = Enum.map(module.destinations, &{name, &1, pulse})
        do_press_button(modules, queue ++ new_pulses, 
          [{from, name, pulse} | processed_pulses], 
          new_pulses ++ acc_pulses)
      
      %{type: :flip_flop, state: current_state} = module when not pulse ->
        new_state = not current_state
        new_modules = put_in(modules[name].state, new_state)
        new_pulses = Enum.map(module.destinations, &{name, &1, new_state})
        do_press_button(new_modules, queue ++ new_pulses, 
          [{from, name, pulse} | processed_pulses], 
          new_pulses ++ acc_pulses)
      
      %{type: :conjunction, inputs: inputs} = module ->
        new_inputs = Map.put(inputs, from, pulse)
        new_modules = put_in(modules[name].inputs, new_inputs)
        
        new_pulse = not Enum.all?(new_inputs, fn {_, v} -> v end)
        new_pulses = Enum.map(module.destinations, &{name, &1, new_pulse})
        
        do_press_button(new_modules, queue ++ new_pulses, 
          [{from, name, pulse} | processed_pulses], 
          new_pulses ++ acc_pulses)
      
      _ -> 
        do_press_button(modules, queue, processed_pulses, acc_pulses)
    end
  end

  defp gcd(a, 0), do: a
  defp gcd(a, b), do: gcd(b, rem(a, b))

  defp lcm(a, b), do: div(a * b, gcd(a, b))
end

# Run the solution
result = Day20.solve("input.txt")
IO.puts(result)
