
defmodule Dance do
  def run do
    # Read the input from the file
    moves = read_input("input.txt")
    
    # Initialize the programs
    programs = Enum.to_list(?a..?p) |> Enum.map(&<<&1>>)

    # Perform the dance moves
    final_order = perform_dance(moves, programs)

    # Print the final order
    IO.puts(Enum.join(final_order, ""))
  end

  defp read_input(file) do
    # Read the contents of the file and split by commas
    File.read!(file)
    |> String.trim()
    |> String.split(",")
  end

  defp perform_dance(moves, programs) do
    Enum.reduce(moves, programs, fn move, acc ->
      case parse_move(move) do
        {:spin, x} -> spin(acc, x)
        {:exchange, a, b} -> exchange(acc, a, b)
        {:partner, a, b} -> partner(acc, a, b)
      end
    end)
  end

  defp parse_move(move) do
    cond do
      String.starts_with?(move, "s") ->
        {:spin, String.to_integer(String.slice(move, 1..-1))}
      
      String.starts_with?(move, "x") ->
        [a, b] = String.split(String.slice(move, 1..-1), "/")
        {:exchange, String.to_integer(a), String.to_integer(b)}
      
      String.starts_with?(move, "p") ->
        [a, b] = String.split(String.slice(move, 1..-1), "/")
        {:partner, a, b}
    end
  end

  defp spin(programs, x) do
    {front, back} = Enum.split(programs, -x)
    back ++ front
  end

  defp exchange(programs, a, b) do
    List.replace_at(List.replace_at(programs, a, Enum.at(programs, b)), b, Enum.at(programs, a))
  end

  defp partner(programs, a, b) do
    a_index = Enum.find_index(programs, fn p -> p == a end)
    b_index = Enum.find_index(programs, fn p -> p == b end)
    exchange(programs, a_index, b_index)
  end
end

# To run the program, call Dance.run()
Dance.run()
