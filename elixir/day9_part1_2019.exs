
defmodule Intcode do
  def run(memory) do
    run(memory, 0, 0, 0)
  end

  defp run(memory, ip, relative_base, output) do
    opcode = rem(Map.get(memory, ip, 0), 100)
    modes = Integer.to_string(div(Map.get(memory, ip, 0), 100))

    get_param = fn offset ->
      mode =
        if String.length(modes) >= offset do
          String.at(modes, String.length(modes) - offset) |> String.to_integer()
        else
          0
        end

      param = Map.get(memory, ip + offset, 0)

      case mode do
        0 -> Map.get(memory, param, 0)
        1 -> param
        2 -> Map.get(memory, relative_base + param, 0)
      end
    end

    set_param = fn offset, value ->
      mode =
        if String.length(modes) >= offset do
          String.at(modes, String.length(modes) - offset) |> String.to_integer()
        else
          0
        end

      param = Map.get(memory, ip + offset, 0)

      case mode do
        0 -> Map.put(memory, param, value)
        2 -> Map.put(memory, relative_base + param, value)
      end
    end

    case opcode do
      1 ->
        memory = set_param.(3, get_param.(1) + get_param.(2))
        run(memory, ip + 4, relative_base, output)
      2 ->
        memory = set_param.(3, get_param.(1) * get_param.(2))
        run(memory, ip + 4, relative_base, output)
      3 ->
        memory = set_param.(1, 1)
        run(memory, ip + 2, relative_base, output)
      4 ->
        output = get_param.(1)
        run(memory, ip + 2, relative_base, output)
      5 ->
        if get_param.(1) != 0 do
          run(memory, get_param.(2), relative_base, output)
        else
          run(memory, ip + 3, relative_base, output)
        end
      6 ->
        if get_param.(1) == 0 do
          run(memory, get_param.(2), relative_base, output)
        else
          run(memory, ip + 3, relative_base, output)
        end
      7 ->
        memory =
          if get_param.(1) < get_param.(2) do
            set_param.(3, 1)
          else
            set_param.(3, 0)
          end
        run(memory, ip + 4, relative_base, output)
      8 ->
        memory =
          if get_param.(1) == get_param.(2) do
            set_param.(3, 1)
          else
            set_param.(3, 0)
          end

        run(memory, ip + 4, relative_base, output)
      9 ->
        run(memory, ip + 2, relative_base + get_param.(1), output)
      99 ->
        output
    end
  end
end

defmodule Solution do
  def solve do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split(",")
    |> Enum.with_index()
    |> Enum.map(fn {value, index} -> {index, String.to_integer(value)} end)
    |> Map.new()
    |> Intcode.run()
    |> IO.puts()
  end
end

Solution.solve()
