
defmodule Intcode do
  defstruct memory: %{}, ip: 0, input: [], output: [], halted: false

  def from_program(program_list) do
    memory = Enum.with_index(program_list) |> Map.new(fn {v, i} -> {i, v} end)
    %__MODULE__{memory: memory}
  end

  def add_input(state, input) do
    %{state | input: state.input ++ [input]}
  end

  def run(state) do
    case state.halted or length(state.output) >= 2 do
      true ->
        state
      false ->
        run(step(state))
    end
  end

  defp step(state) do
    memory = state.memory
    ip = state.ip
    instruction = Map.get(memory, ip, 0)
    opcode = rem(instruction, 100)
    param_modes = div(instruction, 100)

    get_value = fn (param_value, mode) ->
      case mode do
        0 -> read_memory(state, param_value)
        1 -> param_value
        _ -> raise "Unknown parameter mode: #{mode}"
      end
    end

    get_mode = fn (param_index) ->
       div(param_modes, trunc(:math.pow(10, param_index))) |> rem(10)
    end

    case opcode do
      1 ->
        params = get_params(state, 3)
        v1 = get_value.(Enum.at(params, 0), get_mode.(0))
        v2 = get_value.(Enum.at(params, 1), get_mode.(1))
        addr = Enum.at(params, 2)
        write_memory(state, addr, v1 + v2) |> Map.put(:ip, ip + 4)
      2 ->
        params = get_params(state, 3)
        v1 = get_value.(Enum.at(params, 0), get_mode.(0))
        v2 = get_value.(Enum.at(params, 1), get_mode.(1))
        addr = Enum.at(params, 2)
        write_memory(state, addr, v1 * v2) |> Map.put(:ip, ip + 4)
      3 ->
        params = get_params(state, 1)
        addr = Enum.at(params, 0)
        case state.input do
          [] -> state
          [h | t] ->
            write_memory(state, addr, h) |> Map.put(:input, t) |> Map.put(:ip, ip + 2)
        end
      4 ->
        params = get_params(state, 1)
        value = get_value.(Enum.at(params, 0), get_mode.(0))
        %{state | output: state.output ++ [value], ip: ip + 2}
      5 ->
        params = get_params(state, 2)
        v1 = get_value.(Enum.at(params, 0), get_mode.(0))
        target = get_value.(Enum.at(params, 1), get_mode.(1))
        new_ip = if v1 != 0, do: target, else: ip + 3
        Map.put(state, :ip, new_ip)
      6 ->
        params = get_params(state, 2)
        v1 = get_value.(Enum.at(params, 0), get_mode.(0))
        target = get_value.(Enum.at(params, 1), get_mode.(1))
        new_ip = if v1 == 0, do: target, else: ip + 3
        Map.put(state, :ip, new_ip)
      7 ->
        params = get_params(state, 3)
        v1 = get_value.(Enum.at(params, 0), get_mode.(0))
        v2 = get_value.(Enum.at(params, 1), get_mode.(1))
        addr = Enum.at(params, 2)
        value = if v1 < v2, do: 1, else: 0
        write_memory(state, addr, value) |> Map.put(:ip, ip + 4)
      8 ->
        params = get_params(state, 3)
        v1 = get_value.(Enum.at(params, 0), get_mode.(0))
        v2 = get_value.(Enum.at(params, 1), get_mode.(1))
        addr = Enum.at(params, 2)
        value = if v1 == v2, do: 1, else: 0
        write_memory(state, addr, value) |> Map.put(:ip, ip + 4)
      99 ->
        Map.put(state, :halted, true)
      _ ->
        raise "Unknown opcode: #{opcode} at IP #{ip}"
    end
  end

  defp read_memory(state, address), do: Map.get(state.memory, address, 0)

  defp write_memory(state, address, value), do: %{state | memory: Map.put(state.memory, address, value)}

  defp get_params(state, count) do
    memory = state.memory
    ip = state.ip
    0..(count - 1) |> Enum.map(fn i ->
      Map.get(memory, ip + i + 1, 0)
    end)
  end
end

defmodule Robot do
  defstruct pos: {0, 0}, dir: 0 # 0: Up, 1: Right, 2: Down, 3: Left

  def turn_and_move(robot_state, turn_direction) do
    new_dir = case turn_direction do
      0 -> rem(robot_state.dir - 1 + 4, 4)
      1 -> rem(robot_state.dir + 1, 4)
      _ -> raise "Unknown turn direction: #{turn_direction}"
    end

    {x, y} = robot_state.pos
    new_pos = case new_dir do
      0 -> {x, y - 1}
      1 -> {x + 1, y}
      2 -> {x, y + 1}
      3 -> {x - 1, y}
    end

    %__MODULE__{pos: new_pos, dir: new_dir}
  end
end

defmodule Solution do
  def main do
    program_string = File.read!("input.txt")
    program = program_string |> String.trim() |> String.split(",") |> Enum.map(&String.to_integer/1)

    intcode_state = Intcode.from_program(program)
    robot_state = %Robot{}
    grid = %{}

    final_grid = simulate(intcode_state, robot_state, grid)

    IO.puts(Map.size(final_grid))
  end

  defp simulate(intcode_state, robot_state, grid) do
    if intcode_state.halted do
      grid
    else
      current_color = Map.get(grid, robot_state.pos, 0)
      intcode_with_input = Intcode.add_input(intcode_state, current_color)
      new_intcode_state = Intcode.run(intcode_with_input)

      if length(new_intcode_state.output) >= 2 do
        [paint_color, turn_dir] = new_intcode_state.output
        intcode_state_after_output = %{new_intcode_state | output: []}
        new_grid = Map.put(grid, robot_state.pos, paint_color)
        new_robot_state = Robot.turn_and_move(robot_state, turn_dir)
        simulate(intcode_state_after_output, new_robot_state, new_grid)
      else
        if new_intcode_state.halted do
           grid
        else
           raise "Unexpected Intcode state during simulation: #{inspect(new_intcode_state)}"
        end
      end
    end
  end
end

Solution.main()
