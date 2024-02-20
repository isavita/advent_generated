defmodule Day8 do
  def call() do
    input = File.read!("input.txt")
    lines = String.split(input, "\n", trim: true)
    
    total_difference =
      Enum.reduce(lines, 0, fn line, acc ->
        code_length = String.length(line)
        memory_length = calculate_memory_length(line)
        acc + (code_length - memory_length)
      end)
    
    IO.puts(total_difference)
  end

  defp calculate_memory_length(line) do
    # Removing the surrounding quotes
    stripped = String.slice(line, 1, String.length(line) - 2)

    # Replacing escape sequences with single characters
    replaced =
      stripped
      |> String.replace(~r/\\"/, "A")
      |> String.replace(~r/\\\\/, "B")
      |> String.replace(~r/\\x[0-9A-Fa-f]{2}/, "C") # Replace hex escape with a placeholder

    String.length(replaced)
  end
end

Day8.call()

