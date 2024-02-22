defmodule Day25 do
  def call do
    {initial_state, steps, states} = read_input("input.txt")
    checksum = run_turing_machine(initial_state, steps, states)
    IO.puts(checksum)
  end

  defp read_input(file_path) do
    content = File.read!(file_path)
    steps = 12_586_542
    initial_state = :A
    states = parse_states(content)
    {initial_state, steps, states}
  end

  defp parse_states(content) do
    content
    |> String.split("In state ", trim: true)
    |> Enum.drop(1) # Drop the initial part before the first state definition
    |> Enum.map(&parse_state/1)
    |> Enum.into(%{})
  end

  defp parse_state(state_info) do
    [state_letter | actions] = String.split(state_info, ":", parts: 2)
    actions_string = Enum.at(actions, 0)
    
    actions_parsed = actions_string
    |> String.split("If the current value is ", trim: true)
    |> Enum.drop(1) # Drop the initial part before the first action definition
    |> Enum.map(&parse_action/1)

    {String.to_atom(state_letter), actions_parsed}
  end

defp parse_action(action_info) do
  [value | rest] = String.split(action_info, ":", parts: 2)
  rest_string = Enum.at(rest, 0)

  matches = 
    Regex.scan(~r/- Write the value (\d+).*?- Move one slot to the (right|left).*?- Continue with state (\w)./ms, rest_string, capture: :all_but_first)
    |> List.first()

  case matches do
    [write_value, move_direction, continue_state] ->
      move = case move_direction do
        "right" -> 1
        "left" -> -1
      end

      {String.to_integer(value), String.to_integer(write_value), move, String.to_atom(continue_state)}
  end
end

  # The rest of the implementation remains unchanged
  defp run_turing_machine(initial_state, steps, states) do
    final_tape = Enum.reduce(0..steps-1, {%{0 => 0}, 0, initial_state}, fn _, {tape, cursor, state} ->
      actions = Map.get(states, state)
      current_value = Map.get(tape, cursor, 0)
      {_, write_value, move, next_state} = Enum.at(actions, current_value)
      
      new_tape = Map.put(tape, cursor, write_value)
      new_cursor = cursor + move
      {new_tape, new_cursor, next_state}
    end)
    |> elem(0)
    |> Map.values()
    |> Enum.filter(&(&1 == 1))
    |> length()

    final_tape
  end
end

Day25.call()
