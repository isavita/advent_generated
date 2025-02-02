
defmodule Monkey do
  defstruct name: nil, val: nil, has_val: false, left: nil, right: nil, op: nil
end

defmodule Day21 do
  def parse do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.reduce(%{}, fn line, index ->
      [goal, rest] = String.split(line, ": ", parts: 2)

      case Integer.parse(rest) do
        {num, _} ->
          Map.put(index, goal, %Monkey{name: goal, val: num, has_val: true})

        :error ->
          [left, op, right] = String.split(rest, " ", trim: true)
          Map.put(
            index,
            goal,
            %Monkey{name: goal, left: left, right: right, op: op}
          )
      end
    end)
    |> Map.to_list()
    |> Enum.reduce(%{}, fn {k, v}, acc -> Map.put(acc, v.name, v) end)
  end

  def solve(index) do
    index = Map.put(index, "humn", %{index["humn"] | has_val: false})
    index = Map.put(index, "root", %{index["root"] | op: "=="})

    expect(index, index["root"], 0)
  end

  defp solve_monkey(index, monkey) do
    if monkey.has_val do
      {monkey.val, true}
    else
      case {monkey.left, monkey.right} do
        {nil, nil} -> {0, false}
        {left, right} ->
          case {solve_monkey(index, index[left]), solve_monkey(index, index[right])} do
            {{l_val, true}, {r_val, true}} ->
              case monkey.op do
                "+" -> {l_val + r_val, true}
                "-" -> {l_val - r_val, true}
                "*" -> {l_val * r_val, true}
                "/" -> {div(l_val, r_val), true}
                "==" -> if l_val == r_val, do: {0, true}, else: {1, true}
              end

            _ ->
              {0, false}
          end
      end
    end
  end

  defp expect(index, monkey, x) do
    if monkey.name == "humn" do
      x
    else
      case {solve_monkey(index, index[monkey.left]), solve_monkey(index, index[monkey.right])} do
        {{_, false}, {r_val, true}} ->
          case monkey.op do
            "+" -> expect(index, index[monkey.left], x - r_val)
            "-" -> expect(index, index[monkey.left], x + r_val)
            "*" -> expect(index, index[monkey.left], div(x, r_val))
            "/" -> expect(index, index[monkey.left], x * r_val)
            "==" -> expect(index, index[monkey.left], r_val)
          end

        {{l_val, true}, {_, false}} ->
          case monkey.op do
            "+" -> expect(index, index[monkey.right], x - l_val)
            "-" -> expect(index, index[monkey.right], l_val - x)
            "*" -> expect(index, index[monkey.right], div(x, l_val))
            "/" -> expect(index, index[monkey.right], div(l_val, x))
            "==" -> expect(index, index[monkey.right], l_val)
          end

        _ ->
          raise "impossible"
      end
    end
  end
end

index = Day21.parse()
ans = Day21.solve(index)
IO.puts(ans)
