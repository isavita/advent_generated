
defmodule Solution do
  def parse_input do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split/1)
  end

  def get_value(arg, registers) do
    case Integer.parse(arg) do
      {num, _} -> num
      :error -> Map.get(registers, arg, 0)
    end
  end

  def run(instructions) do
    solve(instructions, %{}, 0, nil)
  end

  defp solve(instructions, registers, index, last_sound) do
    if index < 0 or index >= length(instructions) do
      nil
    else
      instruction = Enum.at(instructions, index)
      process_instruction(instruction, instructions, registers, index, last_sound)
    end
  end

  defp process_instruction([cmd, arg1 | rest], instructions, registers, index, last_sound) do
    case cmd do
      "snd" ->
        value = get_value(arg1, registers)
        solve(instructions, registers, index + 1, value)

      "set" ->
        value = get_value(Enum.at(rest, 0), registers)
        solve(instructions, Map.put(registers, arg1, value), index + 1, last_sound)

      "add" ->
        value = get_value(Enum.at(rest, 0), registers)
        solve(instructions, Map.update(registers, arg1, value, &(&1 + value)), index + 1, last_sound)

      "mul" ->
        value = get_value(Enum.at(rest, 0), registers)
        solve(instructions, Map.update(registers, arg1, 0, &(&1 * value)), index + 1, last_sound)

      "mod" ->
        value = get_value(Enum.at(rest, 0), registers)
        solve(instructions, Map.update(registers, arg1, 0, &(rem(&1, value))), index + 1, last_sound)

      "rcv" ->
        if get_value(arg1, registers) != 0 do
          last_sound
        else
          solve(instructions, registers, index + 1, last_sound)
        end

      "jgz" ->
        if get_value(arg1, registers) > 0 do
          value = get_value(Enum.at(rest, 0), registers)
          solve(instructions, registers, index + value, last_sound)
        else
          solve(instructions, registers, index + 1, last_sound)
        end
    end
  end

  def solve do
    parse_input()
    |> run()
    |> IO.puts()
  end
end

Solution.solve()
