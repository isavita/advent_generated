
defmodule Monkey do
  def parse(s) do
    [_, items_line, op_line, div_line, true_line, false_line] = String.split(s, "\n")

    items =
      items_line
      |> String.split(": ")
      |> Enum.at(1)
      |> String.split(", ")
      |> Enum.map(&String.to_integer/1)

    [op, val] =
      op_line
      |> String.split("= old ")
      |> Enum.at(1)
      |> String.split(" ")

    operation =
      case {op, val} do
        {"+", "old"} -> &(&1 + &1)
        {"+", v} -> &(&1 + String.to_integer(v))
        {"*", "old"} -> &(&1 * &1)
        {"*", v} -> &(&1 * String.to_integer(v))
      end

    div =
      div_line
      |> String.split("divisible by ")
      |> Enum.at(1)
      |> String.to_integer()

    next_true =
      true_line
      |> String.split("throw to monkey ")
      |> Enum.at(1)
      |> String.to_integer()

    next_false =
      false_line
      |> String.split("throw to monkey ")
      |> Enum.at(1)
      |> String.to_integer()

    %{
      items: items,
      operation: operation,
      div: div,
      next: [next_true, next_false],
      inspections: 0
    }
  end

  def monkey_business(monkeys, rounds, worry) do
    div = Enum.reduce(monkeys, 1, &(&1.div * &2))

    do_monkey_business(monkeys, rounds, worry, div)
    |> Enum.map(& &1.inspections)
    |> Enum.sort(:desc)
    |> Enum.take(2)
    |> Enum.reduce(&(&1 * &2))
  end

  defp do_monkey_business(monkeys, 0, _, _), do: monkeys

  defp do_monkey_business(monkeys, rounds, worry, div) do
    monkeys =
      Enum.reduce(0..(length(monkeys) - 1), monkeys, fn i, acc ->
        monkey = Enum.at(acc, i)
        process_items(acc, monkey, i, worry, div)
      end)
      do_monkey_business(monkeys, rounds - 1, worry, div)
  end
  
  defp process_items(monkeys, monkey, i, worry, div) do
      Enum.reduce(monkey.items, monkeys, fn item, acc_monkeys ->
        new_item = monkey.operation.(item)
        new_item = if worry, do: rem(new_item, div), else: div(new_item, 3)
        next_monkey_index = if rem(new_item, monkey.div) == 0, do: monkey.next |> Enum.at(0), else: monkey.next |> Enum.at(1)

        acc_monkeys
        |> List.update_at(i, &%{&1 | inspections: &1.inspections + 1, items: []})
        |> List.update_at(next_monkey_index, &%{&1 | items: &1.items ++ [new_item]})
      end)
  end
end

monkeys =
  "input.txt"
  |> File.read!()
  |> String.split("\n\n")
  |> Enum.map(&Monkey.parse/1)

Monkey.monkey_business(monkeys, 20, false)
|> IO.puts()
