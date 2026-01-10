
defmodule Factory do
  @moduledoc """
  Solves the Factory indicator light configuration challenge.
  Uses meet-in-the-middle XOR subset sum to find the minimum number of 
  button presses required for each machine.
  """
  use Bitwise

  @doc """
  Main entry point. Reads input from 'input.txt' and prints the total
  minimum button presses required for all machines to standard output.
  """
  def main do
    input_file = "input.txt"

    if File.exists?(input_file) do
      input_file
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_line/1)
      |> Enum.map(&min_presses/1)
      |> Enum.sum()
      |> IO.puts()
    else
      IO.puts(:stderr, "Error: #{input_file} not found.")
    end
  end

  @doc """
  Parses a single machine line into a target bitmask and a list of button bitmasks.
  """
  def parse_line(line) do
    # Extract indicator light diagram: [###.]
    diagram = Regex.run(~r/\[([.#]+)\]/, line) |> Enum.at(1)
    
    # Convert diagram to bitmask where '.' is 0 and '#' is 1.
    # The first light is the 0-th bit (1 <<< 0).
    target = 
      diagram
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(0, fn {char, i}, acc ->
        if char == "#", do: acc ||| (1 <<< i), else: acc
      end)

    # Extract button wiring: (0,2,3)
    buttons = 
      Regex.scan(~r/\(([\d,]+)\)/, line)
      |> Enum.map(fn [_, indices_str] ->
        indices_str
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)
        |> Enum.reduce(0, fn i, acc -> acc ||| (1 <<< i) end)
      end)

    {target, buttons}
  end

  @doc """
  Finds the minimum number of button presses (XOR subset sum) to reach the target mask.
  Uses a meet-in-the-middle approach to handle machines with up to 40 buttons.
  """
  def min_presses({target, buttons}) do
    n = length(buttons)
    {left_half, right_half} = Enum.split(buttons, div(n, 2))

    # Generate maps of {xor_sum => min_weight} for both halves
    left_sums = generate_sums(left_half)
    right_sums = generate_sums(right_half)

    # For every possible XOR sum in the left half, check if (target ^ left_sum)
    # exists in the right half and find the one that minimizes the total button count.
    Enum.reduce(left_sums, :infinity, fn {l_mask, l_weight}, min_w ->
      needed = l_mask ^^^ target
      case Map.get(right_sums, needed) do
        nil -> min_w
        r_weight ->
          current_total = l_weight + r_weight
          if min_w == :infinity or current_total < min_w, do: current_total, else: min_w
      end
    end)
  end

  @doc """
  Generates all possible XOR sums from a list of button masks and stores 
  the minimum number of buttons used to reach each state.
  """
  def generate_sums(buttons) do
    Enum.reduce(buttons, %{0 => 0}, fn btn, acc ->
      # Create new sums by toggling the current button in all existing states
      new_sums = Map.new(acc, fn {mask, weight} -> {mask ^^^ btn, weight + 1} end)
      
      # Merge new sums into existing map, keeping the minimum weight for each mask
      Map.merge(acc, new_sums, fn _mask, w1, w2 -> if w1 < w2, do: w1, else: w2 end)
    end)
  end
end

# Invoke the program
Factory.main()
