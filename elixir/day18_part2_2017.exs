
defmodule Day18 do
  def solve do
    instructions = 
      File.read!("input.txt")
      |> String.split("\n", trim: true)
      |> Enum.map(&String.split/1)

    run_programs(instructions)
  end

  def get_value(arg, registers) do
    case Integer.parse(arg) do
      {num, _} -> num
      :error -> Map.get(registers, arg, 0)
    end
  end

  def run_programs(instructions) do
    do_run_programs(
      %{registers0: %{"p" => 0}, 
        registers1: %{"p" => 1}, 
        queue0: [], 
        queue1: [], 
        send_count: 0, 
        index0: 0, 
        index1: 0
      }, 
      instructions
    )
  end

  def do_run_programs(state, instructions) do
    case step_program(state, instructions, 0) do
      {:deadlock, new_state} -> 
        case step_program(new_state, instructions, 1) do
          {:deadlock, final_state} -> final_state.send_count
          {:continue, updated_state} -> do_run_programs(updated_state, instructions)
        end
      {:continue, updated_state} -> do_run_programs(updated_state, instructions)
    end
  end

  def step_program(state, instructions, program) do
    {reg_key, queue_send, queue_recv, index_key} = 
      if program == 0, 
        do: {:registers0, :queue1, :queue0, :index0}, 
        else: {:registers1, :queue0, :queue1, :index1}

    registers = Map.get(state, reg_key)
    index = Map.get(state, index_key)

    if index < 0 or index >= length(instructions) do
      {:deadlock, state}
    else
      instruction = Enum.at(instructions, index)
      process_instruction(instruction, state, program, reg_key, queue_send, queue_recv, index_key)
    end
  end

  def process_instruction(["snd", x], state, program, reg_key, queue_send, _, index_key) do
    val = get_value(x, Map.get(state, reg_key))
    new_state = 
      state 
      |> Map.update!(queue_send, fn q -> q ++ [val] end)
      |> Map.update!(:send_count, fn c -> if program == 1, do: c + 1, else: c end)
      |> Map.update!(index_key, &(&1 + 1))
    {:continue, new_state}
  end

  def process_instruction(["set", x, y], state, _, reg_key, _, _, index_key) do
    registers = Map.get(state, reg_key)
    val = get_value(y, registers)
    new_state = 
      state 
      |> Map.put(reg_key, Map.put(registers, x, val))
      |> Map.update!(index_key, &(&1 + 1))
    {:continue, new_state}
  end

  def process_instruction(["add", x, y], state, _, reg_key, _, _, index_key) do
    registers = Map.get(state, reg_key)
    val = get_value(y, registers)
    new_state = 
      state 
      |> Map.put(reg_key, Map.update!(registers, x, fn curr -> curr + val end))
      |> Map.update!(index_key, &(&1 + 1))
    {:continue, new_state}
  end

  def process_instruction(["mul", x, y], state, _, reg_key, _, _, index_key) do
    registers = Map.get(state, reg_key)
    val = get_value(y, registers)
    new_state = 
      state 
      |> Map.put(reg_key, Map.update!(registers, x, fn curr -> curr * val end))
      |> Map.update!(index_key, &(&1 + 1))
    {:continue, new_state}
  end

  def process_instruction(["mod", x, y], state, _, reg_key, _, _, index_key) do
    registers = Map.get(state, reg_key)
    val = get_value(y, registers)
    new_state = 
      state 
      |> Map.put(reg_key, Map.update!(registers, x, fn curr -> rem(curr, val) end))
      |> Map.update!(index_key, &(&1 + 1))
    {:continue, new_state}
  end

  def process_instruction(["rcv", x], state, _, reg_key, _, queue_recv, index_key) do
    registers = Map.get(state, reg_key)
    case Map.get(state, queue_recv) do
      [] -> {:deadlock, state}
      [val | rest] ->
        new_state = 
          state 
          |> Map.put(reg_key, Map.put(registers, x, val))
          |> Map.put(queue_recv, rest)
          |> Map.update!(index_key, &(&1 + 1))
        {:continue, new_state}
    end
  end

  def process_instruction(["jgz", x, y], state, _, reg_key, _, _, index_key) do
    registers = Map.get(state, reg_key)
    if get_value(x, registers) > 0 do
      new_index = Map.get(state, index_key) + get_value(y, registers)
      {:continue, Map.put(state, index_key, new_index)}
    else
      {:continue, Map.update!(state, index_key, &(&1 + 1))}
    end
  end
end

IO.puts(Day18.solve())
