defmodule AdventSolver do
  @moduledoc """
  Intcode emulator and text adventure solver.
  Rewritten from Python to idiomatic Elixir.
  """

  defmodule Room do
    defstruct name: nil, connections: %{}

    @type t :: %__MODULE__{
            name: String.t(),
            connections: %{String.t() => t() | nil}
          }
  end

  defmodule Emulator do
    defstruct memory: %{}, input: [], ip: 0, relative_base: 0

    @type t :: %__MODULE__{
            memory: %{non_neg_integer() => integer()},
            input: [integer()],
            ip: non_neg_integer(),
            relative_base: integer()
          }

    @spec new([integer()]) :: t()
    def new(program) do
      memory =
        program
        |> Enum.with_index()
        |> Enum.into(%{}, fn {val, idx} -> {idx, val} end)

      %__MODULE__{memory: memory}
    end

    @spec write_string(t(), String.t()) :: t()
    def write_string(%__MODULE__{input: input} = emu, string) do
      chars = String.to_charlist(string)
      %{emu | input: Enum.concat(input, chars)}
    end

    @spec emulate(t()) :: {t(), {:output, integer()} | :halted | :waiting_for_input}
    def emulate(%__MODULE__{} = emu) do
      run_loop(emu)
    end

    defp run_loop(%__MODULE__{memory: mem, ip: ip} = emu) do
      instruction = Map.get(mem, ip, 0)
      opcode = rem(instruction, 100)

      case opcode do
        1 -> execute_binop(emu, instruction, &+/2)
        2 -> execute_binop(emu, instruction, &*/2)
        3 -> execute_input(emu, instruction)
        4 -> execute_output(emu, instruction)
        5 -> execute_jump(emu, instruction, &(&1 != 0))
        6 -> execute_jump(emu, instruction, &(&1 == 0))
        7 -> execute_compare(emu, instruction, &</2)
        8 -> execute_compare(emu, instruction, &==/2)
        9 -> execute_adjust_base(emu, instruction)
        99 -> {emu, :halted}
        _ -> raise "Unknown opcode: #{opcode} at position #{ip}"
      end
    end

    defp get_parameter(%__MODULE__{memory: mem, ip: ip, relative_base: rb}, instruction, offset) do
      mode = div(instruction, power_of_10(offset + 1)) |> rem(10)
      param = Map.get(mem, ip + offset, 0)

      case mode do
        0 -> Map.get(mem, param, 0)
        1 -> param
        2 -> Map.get(mem, rb + param, 0)
        _ -> raise "Unknown parameter mode: #{mode}"
      end
    end

    defp get_write_address(%__MODULE__{ip: ip, relative_base: rb, memory: mem}, instruction, offset) do
      mode = div(instruction, power_of_10(offset + 1)) |> rem(10)
      param = Map.get(mem, ip + offset, 0)

      case mode do
        0 -> param
        2 -> rb + param
        _ -> raise "Invalid mode for writing: #{mode}"
      end
    end

    defp power_of_10(0), do: 1
    defp power_of_10(1), do: 10
    defp power_of_10(2), do: 100
    defp power_of_10(3), do: 1000
    defp power_of_10(4), do: 10000

    defp execute_binop(emu, instruction, op) do
      a = get_parameter(emu, instruction, 1)
      b = get_parameter(emu, instruction, 2)
      addr = get_write_address(emu, instruction, 3)

      emu
      |> put_in([Access.key(:memory), addr], op.(a, b))
      |> Map.update!(:ip, &(&1 + 4))
      |> run_loop()
    end

    defp execute_input(%__MODULE__{input: []} = emu, _instruction) do
      {emu, :waiting_for_input}
    end

    defp execute_input(%__MODULE__{input: [val | rest]} = emu, instruction) do
      addr = get_write_address(emu, instruction, 1)

      %{emu | input: rest}
      |> put_in([Access.key(:memory), addr], val)
      |> Map.update!(:ip, &(&1 + 2))
      |> run_loop()
    end

    defp execute_output(emu, instruction) do
      a = get_parameter(emu, instruction, 1)
      emu = Map.update!(emu, :ip, &(&1 + 2))
      {emu, {:output, a}}
    end

    defp execute_jump(emu, instruction, condition) do
      a = get_parameter(emu, instruction, 1)
      b = get_parameter(emu, instruction, 2)

      new_ip = if condition.(a), do: b, else: emu.ip + 3

      %{emu | ip: new_ip}
      |> run_loop()
    end

    defp execute_compare(emu, instruction, comparator) do
      a = get_parameter(emu, instruction, 1)
      b = get_parameter(emu, instruction, 2)
      addr = get_write_address(emu, instruction, 3)
      result = if comparator.(a, b), do: 1, else: 0

      emu
      |> put_in([Access.key(:memory), addr], result)
      |> Map.update!(:ip, &(&1 + 4))
      |> run_loop()
    end

    defp execute_adjust_base(emu, instruction) do
      a = get_parameter(emu, instruction, 1)

      emu
      |> Map.update!(:relative_base, &(&1 + a))
      |> Map.update!(:ip, &(&1 + 2))
      |> run_loop()
    end
  end

  defmodule State do
    defstruct [
      :emulator,
      :current_room,
      :checkpoint,
      :floor,
      :test_dir,
      :last,
      world: %{},
      inventory: %{},
      mode: :explore,
      path: [],
      available_items: [],
      item_mask: 0,
      last_items: [],
      last_dir: "",
      output_buffer: [],
      action_history: [],
      stuck_counter: 0
    ]
  end

  @opposite %{
    "north" => "south",
    "south" => "north",
    "west" => "east",
    "east" => "west"
  }

  # IMPORTANT: ~w(...) splits on whitespace, so it cannot represent multi-word items.
  @blacklist MapSet.new([
               "photons",
               "escape pod",
               "molten lava",
               "infinite loop",
               "giant electromagnet"
             ])

  defp blacklisted?(item), do: MapSet.member?(@blacklist, item)

  def run(filename \\ "input.txt") do
    program =
      filename
      |> File.read!()
      |> String.trim()
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    emulator = Emulator.new(program)
    state = %State{emulator: emulator}

    game_loop(state)
  end

  defp game_loop(state, iteration \\ 0) do
    if iteration > 500 do
      IO.puts(:stderr, "LOOP DETECTED after #{iteration} iterations")
      IO.puts(:stderr, "Mode: #{state.mode}")
      IO.puts(:stderr, "Current room: #{state.current_room && state.current_room.name}")
      IO.puts(:stderr, "Path length: #{length(state.path)}")
      IO.puts(:stderr, "Item mask: #{state.item_mask}")
      IO.puts(:stderr, "Inventory: #{inspect(state.inventory)}")
      IO.puts(:stderr, "Available items: #{inspect(state.available_items)}")
      IO.puts(:stderr, "Test dir: #{inspect(state.test_dir)}")
      IO.puts(:stderr, "World rooms: #{inspect(Map.keys(state.world))}")
      {:error, "Loop detected", ""}
    else
      {emulator, result} = Emulator.emulate(state.emulator)
      state = %{state | emulator: emulator}

      case result do
        :halted ->
          handle_halted(state)

        {:output, char} ->
          state
          |> Map.update!(:output_buffer, &[char | &1])
          |> game_loop(iteration)

        :waiting_for_input ->
          {state, items} = process_output(state)

          state
          |> execute_action(items)
          |> game_loop(iteration + 1)
      end
    end
  end

  defp handle_halted(state) do
    output = state.output_buffer |> Enum.reverse() |> :binary.list_to_bin()

    case Regex.run(
           ~r/"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."/,
           output
         ) do
      [_, code] -> code
      _ -> {:error, "No solution found", output}
    end
  end

  defp process_output(state) do
    output = state.output_buffer |> Enum.reverse() |> :binary.list_to_bin()
    state = %{state | output_buffer: []}

    if length(state.action_history) < 20 do
      IO.puts(
        :stderr,
        "=== RAW OUTPUT (action #{length(state.action_history)}, last=#{state.last && state.last.name}, last_dir=#{state.last_dir}) ==="
      )

      IO.puts(:stderr, output)
      IO.puts(:stderr, "=== END OUTPUT ===")
    end

    lines = String.split(output, "\n")
    parse_lines(state, lines, [])
  end

  defp parse_lines(state, [], items), do: {state, items}

  defp parse_lines(state, [line | rest], items) do
    line = String.trim(line)

    if String.contains?(line, "Alert") or String.contains?(line, "robotic") do
      IO.puts(:stderr, "POTENTIAL ALERT LINE: #{inspect(line)}")
    end

    cond do
      line == "" or line == "Command?" ->
        parse_lines(state, rest, items)

      String.match?(line, ~r/^== (.+) ==$/) ->
        [_, name] = Regex.run(~r/^== (.+) ==$/, line)
        rest = Enum.drop_while(rest, &(String.trim(&1) != ""))

        {room, world} =
          case Map.get(state.world, name) do
            nil ->
              room = %Room{name: name}
              {room, Map.put(state.world, name, room)}

            existing ->
              {existing, state.world}
          end

        state = %{state | current_room: room, world: world}
        parse_lines(state, rest, [])

      line == "Doors here lead:" ->
        {state, rest} = parse_doors(state, rest)
        parse_lines(state, rest, items)

      line == "Items here:" ->
        {items, rest} = parse_items(rest, [])
        parse_lines(state, rest, items)

      String.match?(line, ~r/^You take the (.+)\.$/) ->
        [_, taken] = Regex.run(~r/^You take the (.+)\.$/, line)
        state = put_in(state.inventory[taken], true)

        updated_items = Enum.reject(state.last_items, &(&1 == taken))

        state =
          if state.last do
            %{state | current_room: state.last}
          else
            state
          end

        parse_lines(state, rest, updated_items)

      String.match?(line, ~r/^You drop the (.+)\.$/) ->
        [_, dropped] = Regex.run(~r/^You drop the (.+)\.$/, line)
        state = put_in(state.inventory[dropped], false)

        updated_items = state.last_items ++ [dropped]

        state =
          if state.last do
            %{state | current_room: state.last}
          else
            state
          end

        parse_lines(state, rest, updated_items)

      String.starts_with?(line, "A loud, robotic voice says \"Alert!") ->
        state = handle_security_alert(state)
        parse_lines(state, rest, items)

      true ->
        parse_lines(state, rest, items)
    end
  end

  defp parse_doors(state, lines) do
    {door_lines, rest} = Enum.split_while(lines, &(String.trim(&1) != ""))

    room =
      Enum.reduce(door_lines, state.current_room, fn line, room ->
        case Regex.run(~r/^- (.+)$/, String.trim(line)) do
          [_, direction] ->
            if Map.has_key?(room.connections, direction) do
              room
            else
              %{room | connections: Map.put(room.connections, direction, nil)}
            end

          _ ->
            room
        end
      end)

    world = Map.put(state.world, room.name, room)
    {%{state | current_room: room, world: world}, rest}
  end

  defp parse_items(lines, acc) do
    case lines do
      [line | rest] ->
        trimmed = String.trim(line)

        if trimmed == "" do
          {Enum.reverse(acc), lines}
        else
          case Regex.run(~r/^- (.+)$/, trimmed) do
            [_, item] -> parse_items(rest, [item | acc])
            _ -> parse_items(rest, acc)
          end
        end

      [] ->
        {Enum.reverse(acc), []}
    end
  end

  defp handle_security_alert(state) do
    IO.puts(
      :stderr,
      "*** SECURITY ALERT! last=#{state.last && state.last.name}, last_dir=#{state.last_dir}, current=#{state.current_room && state.current_room.name}"
    )

    state =
      if state.mode == :explore do
        path =
          case state.path do
            [] -> []
            [_ | rest] -> rest
          end

        checkpoint = state.last
        floor = state.current_room
        test_dir = state.last_dir

        state =
          if checkpoint && test_dir != "" do
            updated_checkpoint = %{
              checkpoint
              | connections: Map.put(checkpoint.connections, test_dir, floor)
            }

            world = Map.put(state.world, checkpoint.name, updated_checkpoint)
            %{state | world: world, checkpoint: updated_checkpoint}
          else
            %{state | checkpoint: checkpoint}
          end

        %{state | path: path, floor: floor, test_dir: test_dir}
      else
        state
      end

    %{state | last: nil, last_items: [], last_dir: ""}
  end

  defp execute_action(state, items) do
    state = update_connections(state)

    state = %{state | last: state.current_room, last_items: items, last_dir: ""}

    case state.mode do
      :explore -> explore(state)
      :navigate -> navigate(state)
      :test -> test_items(state)
    end
  end

  defp update_connections(state) do
    if state.last && state.last_dir != "" && state.current_room do
      dir = state.last_dir

      last = Map.get(state.world, state.last.name, state.last)
      current = Map.get(state.world, state.current_room.name, state.current_room)

      if Map.get(last.connections, dir) == nil do
        IO.puts(:stderr, "CONNECT: #{last.name} --#{dir}--> #{current.name}")
        updated_last = %{last | connections: Map.put(last.connections, dir, current)}

        updated_current = %{
          current
          | connections: Map.put(current.connections, @opposite[dir], updated_last)
        }

        world =
          state.world
          |> Map.put(updated_last.name, updated_last)
          |> Map.put(updated_current.name, updated_current)

        %{state | world: world, current_room: updated_current, last: updated_last}
      else
        state
      end
    else
      state
    end
  end

  defp explore(state) do
    items = state.last_items

    # Try to pick up safe items (do NOT take "infinite loop", etc.)
    case Enum.find(items, fn item -> not blacklisted?(item) end) do
      nil ->
        explore_rooms(state)

      item ->
        IO.puts(:stderr, "Taking item: #{item}")
        send_command(state, "take #{item}\n")
    end
  end

  defp explore_rooms(state) do
    room = Map.get(state.world, state.current_room.name, state.current_room)

    unexplored = Enum.filter(room.connections, fn {_dir, dest} -> dest == nil end)

    case unexplored do
      [{direction, _} | _] ->
        IO.puts(:stderr, "EXPLORE: #{room.name} -> #{direction}")
        state = %{state | path: [room | state.path], last_dir: direction}
        send_command(state, "#{direction}\n")

      [] ->
        IO.puts(:stderr, "BACKTRACK from #{room.name}")
        backtrack(state)
    end
  end

  defp backtrack(state) do
    case state.path do
      [] ->
        if state.checkpoint && state.floor do
          IO.puts(
            :stderr,
            "*** EXPLORATION COMPLETE! Checkpoint: #{state.checkpoint.name}, Floor: #{state.floor.name}"
          )

          path = find_path(state.current_room, state.checkpoint, state.world)
          path = if path, do: tl(path), else: []
          %{state | mode: :navigate, path: path}
        else
          IO.puts(
            :stderr,
            "ERROR: No checkpoint! checkpoint=#{inspect(state.checkpoint)}, floor=#{inspect(state.floor)}"
          )

          raise "No checkpoint found"
        end

      [last_room | rest_path] ->
        current = Map.get(state.world, state.current_room.name, state.current_room)

        direction =
          current.connections
          |> Enum.find_value(fn {dir, dest} ->
            if dest && dest.name == last_room.name, do: dir
          end)

        if direction do
          IO.puts(:stderr, "BACKTRACK: #{current.name} -> #{direction} (to #{last_room.name})")
          state = %{state | path: rest_path, last_dir: direction}
          send_command(state, "#{direction}\n")
        else
          raise "Cannot go from \"#{state.current_room.name}\" to \"#{last_room.name}\""
        end
    end
  end

  defp navigate(state) do
    case state.path do
      [] ->
        available = for {item, true} <- state.inventory, do: item
        IO.puts(:stderr, "*** TEST MODE: items=#{inspect(available)}, test_dir=#{state.test_dir}")
        %{state | mode: :test, available_items: available, item_mask: 0}

      [next_room | rest] ->
        current = Map.get(state.world, state.current_room.name, state.current_room)

        direction =
          current.connections
          |> Enum.find_value(fn {dir, dest} ->
            if dest && dest.name == next_room.name, do: dir
          end)

        if direction do
          state = %{state | path: rest, last_dir: direction}
          send_command(state, "#{direction}\n")
        else
          raise "Cannot go from \"#{state.current_room.name}\" to \"#{next_room.name}\""
        end
    end
  end

  defp test_items(state) do
    items = state.available_items
    mask = state.item_mask
    max_mask = Bitwise.bsl(1, length(items))

    if mask >= max_mask do
      raise "No valid item combination found after #{max_mask} attempts"
    end

    item_to_change =
      items
      |> Enum.with_index()
      |> Enum.find(fn {item, idx} ->
        target_state = Bitwise.band(mask, Bitwise.bsl(1, idx)) != 0
        state.inventory[item] != target_state
      end)

    case item_to_change do
      nil ->
        state = %{state | item_mask: mask + 1}

        if state.test_dir && state.test_dir != "" do
          send_command(state, "#{state.test_dir}\n")
        else
          raise "Test direction not set"
        end

      {item, idx} ->
        target_state = Bitwise.band(mask, Bitwise.bsl(1, idx)) != 0
        action = if target_state, do: "take", else: "drop"
        send_command(state, "#{action} #{item}\n")
    end
  end

  defp send_command(state, command) do
    emulator = Emulator.write_string(state.emulator, command)

    action_key = {state.mode, state.current_room && state.current_room.name, String.trim(command)}
    history = [action_key | Enum.take(state.action_history, 99)]

    %{state | emulator: emulator, action_history: history}
  end

  defp find_path(from_room, to_room, world) do
    from_name = from_room.name
    to_name = to_room.name
    from_fresh = Map.get(world, from_name)
    bfs(:queue.from_list([{from_name, [from_fresh]}]), MapSet.new([from_name]), to_name, world)
  end

  defp bfs(queue, visited, target_name, world) do
    case :queue.out(queue) do
      {:empty, _} ->
        nil

      {{:value, {current_name, rev_path}}, rest_queue} ->
        if current_name == target_name do
          Enum.reverse(rev_path)
        else
          current = Map.get(world, current_name)

          {new_queue, new_visited} =
            current.connections
            |> Enum.filter(fn {_dir, room} -> room && room.name not in visited end)
            |> Enum.reduce({rest_queue, visited}, fn {_dir, room}, {q, v} ->
              fresh_room = Map.get(world, room.name)
              new_q = :queue.in({room.name, [fresh_room | rev_path]}, q)
              new_v = MapSet.put(v, room.name)
              {new_q, new_v}
            end)

          bfs(new_queue, new_visited, target_name, world)
        end
    end
  end
end

defmodule Main do
  def run do
    case AdventSolver.run() do
      code when is_binary(code) ->
        IO.puts(code)

      {:error, msg, output} ->
        IO.puts(:stderr, "Error: #{msg}")
        IO.puts(:stderr, output)
    end
  end
end

Main.run()
