defmodule Day7 do
  def call() do
    input = File.read!("input.txt")
    bottom_program = find_bottom_program(input)
    IO.puts(bottom_program)
  end

  defp find_bottom_program(input) do
    {holders, held} = input
                      |> String.split("\n", trim: true)
                      |> Enum.reduce({%{}, %{}}, &process_line/2)

    # The bottom program is the one that holds others but is not held by anyone.
    Enum.find(holders, fn {program, _} -> not Map.has_key?(held, program) end)
    |> elem(0)
  end

  defp process_line(line, {holders, held}) do
    [program_info | children_info] = String.split(line, " -> ", trim: true)
    [program_name | _] = Regex.run(~r/^\w+/, program_info)
    
    children = case children_info do
      [] -> []
      [children_str] -> String.split(children_str, ", ")
    end

    # Update holders: program holding other programs.
    holders = Map.put(holders, program_name, children)

    # Update held: programs that are held by others.
    held = Enum.reduce(children, held, fn child, acc ->
      Map.put(acc, child, true)
    end)

    {holders, held}
  end
end

Day7.call()
