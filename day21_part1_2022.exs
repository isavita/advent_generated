
defmodule MonkeyMath do
  def call do
    input = File.read!("input.txt")
    monkey_jobs = input |> String.split("\n") |> Enum.map(&String.split(&1, ": "))
    monkey_numbers = Map.new(monkey_jobs, fn [name, job] -> {name, job} end)

    calculate(monkey_numbers, "root")
  end

  defp calculate(monkey_numbers, name) do
    job = Map.get(monkey_numbers, name)

    case String.split(job, " ") do
      [number] -> String.to_integer(number)
      [op1, "+", op2] -> calculate(monkey_numbers, op1) + calculate(monkey_numbers, op2)
      [op1, "-", op2] -> calculate(monkey_numbers, op1) - calculate(monkey_numbers, op2)
      [op1, "*", op2] -> calculate(monkey_numbers, op1) * calculate(monkey_numbers, op2)
      [op1, "/", op2] -> calculate(monkey_numbers, op1) / calculate(monkey_numbers, op2)
    end
  end
end
