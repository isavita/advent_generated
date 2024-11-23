
defmodule MonkeyInMiddle do
  def solve do
    monkeys = parse_input("input.txt")
    run_rounds(monkeys, 10000, true)
  end

  def parse_input(path) do
    path
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> Enum.map(&parse_monkey/1)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {monkey, index}, acc -> Map.put(acc, index, monkey) end)
  end

  def parse_monkey(monkey_desc) do
    lines = String.split(monkey_desc, "\n")
    
    items = 
      lines 
      |> Enum.at(1) 
      |> String.split(": ") 
      |> List.last() 
      |> String.split(", ") 
      |> Enum.map(&String.to_integer/1)

    [op1, operator, op2] = 
      lines 
      |> Enum.at(2) 
      |> String.split("= ") 
      |> List.last() 
      |> String.split()

    operation = create_operation(operator, op1, op2)
    
    divisible_by = 
      lines 
      |> Enum.at(3) 
      |> String.split() 
      |> List.last() 
      |> String.to_integer()
    
    true_target = 
      lines 
      |> Enum.at(4) 
      |> String.split() 
      |> List.last() 
      |> String.to_integer()
    
    false_target = 
      lines 
      |> Enum.at(5) 
      |> String.split() 
      |> List.last() 
      |> String.to_integer()

    %{
      items: items,
      operation: operation,
      divisible_by: divisible_by,
      targets: {true_target, false_target},
      inspections: 0
    }
  end

  defp create_operation("+", "old", "old"), do: fn x -> x + x end
  defp create_operation("+", "old", n), do: fn x -> x + String.to_integer(n) end
  defp create_operation("*", "old", "old"), do: fn x -> x * x end
  defp create_operation("*", "old", n), do: fn x -> x * String.to_integer(n) end

  def run_rounds(monkeys, rounds, worry_management) do
    divisor = calculate_divisor(monkeys)
    
    final_monkeys = 
      Enum.reduce(1..rounds, monkeys, fn _, acc -> 
        process_round(acc, divisor, worry_management) 
      end)

    final_monkeys
    |> Map.values()
    |> Enum.map(& &1.inspections)
    |> Enum.sort(:desc)
    |> Enum.take(2)
    |> Enum.product()
  end

  defp calculate_divisor(monkeys) do
    monkeys
    |> Map.values()
    |> Enum.map(& &1.divisible_by)
    |> Enum.product()
  end

  defp process_round(monkeys, divisor, worry_management) do
    Enum.reduce(0..(map_size(monkeys) - 1), monkeys, fn monkey_index, acc ->
      process_monkey(acc, monkey_index, divisor, worry_management)
    end)
  end

  defp process_monkey(monkeys, monkey_index, divisor, worry_management) do
    monkey = Map.get(monkeys, monkey_index)
    
    Enum.reduce(monkey.items, monkeys, fn item, acc ->
      new_worry = 
        monkey.operation.(item)
        |> then(fn w -> 
          if worry_management, do: rem(w, divisor), else: div(w, 3) 
        end)
      
      target = 
        if rem(new_worry, monkey.divisible_by) == 0, 
          do: elem(monkey.targets, 0), 
          else: elem(monkey.targets, 1)
      
      acc
      |> update_in([monkey_index, :items], &List.delete(&1, item))
      |> update_in([monkey_index, :inspections], &(&1 + 1))
      |> update_in([target, :items], fn items -> items ++ [new_worry] end)
    end)
  end
end

# Run the solution
IO.puts(MonkeyInMiddle.solve())
