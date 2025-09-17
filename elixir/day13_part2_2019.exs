# Elixir translation optimized for the described problem

defmodule Intcode do
  defstruct memory: %{}, ip: 0, rb: 0, input_queue: [], halted: false

  def new(program) do
    mem =
      program
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {v, i}, acc -> Map.put(acc, i, v) end)

    %Intcode{memory: mem, ip: 0, rb: 0, input_queue: [], halted: false}
  end

  def set_memory(state, addr, val) do
    %{state | memory: Map.put(state.memory, addr, val)}
  end

  def enqueue_input(state, val) do
    %{state | input_queue: state.input_queue ++ [val]}
  end

  def run(state) do
    cond do
      state.halted ->
        {:halt, state}

      true ->
        case step(state) do
          {:cont, s} -> run(s)
          {:output, s, v} -> {:output, s, v}
          {:wait, s} -> {:wait, s}
          {:halt, s} -> {:halt, s}
        end
    end
  end

  defp step(state) do
    instr = Map.get(state.memory, state.ip, 0)
    opcode = rem(instr, 100)
    m1 = rem(div(instr, 100), 10)
    m2 = rem(div(instr, 1000), 10)
    m3 = rem(div(instr, 10000), 10)

    case opcode do
      1 ->
        a = get_param(state, 1, m1)
        b = get_param(state, 2, m2)
        dest = write_addr(state, 3, m3)
        state2 = put_mem(state, dest, a + b)
        {:cont, %{state2 | ip: state.ip + 4}}

      2 ->
        a = get_param(state, 1, m1)
        b = get_param(state, 2, m2)
        dest = write_addr(state, 3, m3)
        state2 = put_mem(state, dest, a * b)
        {:cont, %{state2 | ip: state.ip + 4}}

      3 ->
        case state.input_queue do
          [] ->
            {:wait, state}
          [val | rest] ->
            dest = write_addr(state, 1, m1)
            state2 = put_mem(%{state | input_queue: rest}, dest, val)
            {:cont, %{state2 | ip: state.ip + 2}}
        end

      4 ->
        val = get_param(state, 1, m1)
        state2 = %{state | ip: state.ip + 2}
        {:output, state2, val}

      5 ->
        v1 = get_param(state, 1, m1)
        v2 = get_param(state, 2, m2)
        if v1 != 0, do: {:cont, %{state | ip: v2}}, else: {:cont, %{state | ip: state.ip + 3}}

      6 ->
        v1 = get_param(state, 1, m1)
        v2 = get_param(state, 2, m2)
        if v1 == 0, do: {:cont, %{state | ip: v2}}, else: {:cont, %{state | ip: state.ip + 3}}

      7 ->
        v1 = get_param(state, 1, m1)
        v2 = get_param(state, 2, m2)
        dest = write_addr(state, 3, m3)
        val = if v1 < v2, do: 1, else: 0
        state2 = put_mem(state, dest, val)
        {:cont, %{state2 | ip: state.ip + 4}}

      8 ->
        v1 = get_param(state, 1, m1)
        v2 = get_param(state, 2, m2)
        dest = write_addr(state, 3, m3)
        val = if v1 == v2, do: 1, else: 0
        state2 = put_mem(state, dest, val)
        {:cont, %{state2 | ip: state.ip + 4}}

      9 ->
        v1 = get_param(state, 1, m1)
        {:cont, %{state | rb: state.rb + v1, ip: state.ip + 2}}

      99 ->
        {:halt, %{state | halted: true}}

      _ ->
        {:halt, %{state | halted: true}}
    end
  end

  defp get_param(state, offset, mode) do
    addr = Map.get(state.memory, state.ip + offset, 0)

    case mode do
      0 -> Map.get(state.memory, addr, 0)
      1 -> addr
      2 -> Map.get(state.memory, state.rb + addr, 0)
    end
  end

  defp write_addr(state, offset, mode) do
    addr = Map.get(state.memory, state.ip + offset, 0)

    case mode do
      0 -> addr
      2 -> state.rb + addr
      _ -> addr
    end
  end

  defp put_mem(state, addr, val) do
    %{state | memory: Map.put(state.memory, addr, val)}
  end
end

defmodule Breakout do
  def play_game(program) do
    state = Intcode.new(program) |> Intcode.set_memory(0, 2)
    game_loop(state, 0, 0, 0)
  end

  defp game_loop(state, score, ball_x, paddle_x) do
    case Intcode.run(state) do
      {:halt, _st} ->
        score

      {:wait, st1} ->
        input =
          cond do
            ball_x > paddle_x -> 1
            ball_x < paddle_x -> -1
            true -> 0
          end

        st2 = Intcode.enqueue_input(st1, input)
        game_loop(st2, score, ball_x, paddle_x)

      {:output, st1, x} ->
        case Intcode.run(st1) do
          {:output, st2, y} ->
            case Intcode.run(st2) do
              {:output, st3, tile} ->
                if x == -1 and y == 0 do
                  game_loop(st3, tile, ball_x, paddle_x)
                else
                  new_score = score
                  new_ball_x = if tile == 4, do: x, else: ball_x
                  new_paddle_x = if tile == 3, do: x, else: paddle_x
                  game_loop(st3, new_score, new_ball_x, new_paddle_x)
                end

              {:halt, st3} ->
                score

              {:wait, st3} ->
                game_loop(st3, score, ball_x, paddle_x)
            end

          {:halt, _st2} ->
            score

          {:wait, _st2} ->
            game_loop(state, score, ball_x, paddle_x)
        end
    end
  end
end

defmodule Main do
  def main do
    program = read_input("input.txt")
    score = Breakout.play_game(program)
    IO.puts(score)
  end

  defp read_input(path) do
    content = File.read!(path) |> String.trim()
    content
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
  end
end

Main.main()