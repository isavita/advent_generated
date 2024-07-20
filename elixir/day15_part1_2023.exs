
defmodule LensLibrary do
  # Function to read the input from the file
  def read_input(file) do
    case File.read(file) do
      {:ok, content} -> 
        content |> String.trim() |> String.split(",")
      {:error, reason} -> 
        IO.puts("Error reading file: #{reason}")
        []
    end
  end

  # Function to calculate the HASH value of a given string
  def hash_value(step) do
    step
    |> String.to_charlist()  # Convert string to charlist
    |> Enum.reduce(0, fn char, acc ->
      acc
      |> Kernel.+(char)        # Increase by ASCII code
      |> Kernel.*(17)          # Multiply by 17
      |> rem(256)              # Take remainder of 256
    end)
  end

  # Function to calculate the sum of HASH values for all steps
  def sum_hashes(steps) do
    steps
    |> Enum.map(&hash_value/1)  # Calculate HASH for each step
    |> Enum.sum()               # Sum all HASH values
  end

  # Main function to run the program
  def main do
    steps = read_input("input.txt")
    total_sum = sum_hashes(steps)
    IO.puts("The sum of the HASH values is: #{total_sum}")
  end
end

# Run the program
LensLibrary.main()
