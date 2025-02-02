
defmodule Intcode do
  def run(program, inputs, pc \\ 0, input_index \\ 0) do
    case Enum.at(program, pc) do
      99 ->
        {:halt, program}

      code ->
        opcode = rem(code, 100)
        modes = div(code, 100) |> Integer.digits() |> Enum.reverse() |> Enum.concat([0, 0, 0])

        get_param = fn index ->
          mode = Enum.at(modes, index - 1)
          value = Enum.at(program, pc + index)

          case mode do
            0 -> Enum.at(program, value, 0)
            1 -> value
          end
        end

        case opcode do
          1 ->
            param1 = get_param.(1)
            param2 = get_param.(2)
            dest = Enum.at(program, pc + 3)
            new_program = List.replace_at(program, dest, param1 + param2)
            run(new_program, inputs, pc + 4, input_index)

          2 ->
            param1 = get_param.(1)
            param2 = get_param.(2)
            dest = Enum.at(program, pc + 3)
            new_program = List.replace_at(program, dest, param1 * param2)
            run(new_program, inputs, pc + 4, input_index)

          3 ->
            dest = Enum.at(program, pc + 1)
            new_program = List.replace_at(program, dest, Enum.at(inputs, input_index))
            run(new_program, inputs, pc + 2, input_index + 1)

          4 ->
            param1 = get_param.(1)
            {:output, param1, program, pc + 2, input_index}

          5 ->
            param1 = get_param.(1)
            param2 = get_param.(2)

            if param1 != 0 do
              run(program, inputs, param2, input_index)
            else
              run(program, inputs, pc + 3, input_index)
            end

          6 ->
            param1 = get_param.(1)
            param2 = get_param.(2)

            if param1 == 0 do
              run(program, inputs, param2, input_index)
            else
              run(program, inputs, pc + 3, input_index)
            end

          7 ->
            param1 = get_param.(1)
            param2 = get_param.(2)
            dest = Enum.at(program, pc + 3)
            new_program = List.replace_at(program, dest, if(param1 < param2, do: 1, else: 0))
            run(new_program, inputs, pc + 4, input_index)

          8 ->
            param1 = get_param.(1)
            param2 = get_param.(2)
            dest = Enum.at(program, pc + 3)
            new_program = List.replace_at(program, dest, if(param1 == param2, do: 1, else: 0))
            run(new_program, inputs, pc + 4, input_index)
        end
    end
  end
end

defmodule Amplifier do
  def run_sequence(program, phases) do
    phases
    |> Enum.reduce(0, fn phase, input ->
      result = Intcode.run(program, [phase, input])

      case result do
        {:output, output, _, _, _} -> output
        _ -> raise "Unexpected Intcode result"
      end
    end)
  end

  def find_max_thruster_signal(program) do
    [0, 1, 2, 3, 4]
    |> Enum.to_list()
    |> then(&Enum.map(permute(&1), fn p -> {p, run_sequence(program, p)} end))
    |> Enum.max_by(fn {_, signal} -> signal end)
    |> elem(1)
  end

  defp permute([]), do: [[]]

  defp permute(list) do
    for x <- list, rest <- permute(list -- [x]), do: [x | rest]
  end
end

program =
  File.read!("input.txt")
  |> String.trim()
  |> String.split(",")
  |> Enum.map(&String.to_integer/1)

max_signal = Amplifier.find_max_thruster_signal(program)
IO.puts(max_signal)
