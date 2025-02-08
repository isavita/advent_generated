
defmodule JumbledSevenSegment do
  def solve() do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> Enum.map(&decode_output/1)
    |> Enum.sum()
    |> IO.puts()
  end

  defp parse_line(line) do
    line
    |> String.split(" | ")
    |> Enum.map(&String.split(&1, " "))
  end
  
  defp decode_output([patterns, output]) do
    patterns = Enum.map(patterns, &to_charlist/1)
    patterns = Enum.map(patterns, &Enum.sort/1)
   
    output = Enum.map(output, &to_charlist/1)   
    output = Enum.map(output, &Enum.sort/1)

    mapping = deduce_mapping(patterns)

    Enum.reduce(output, 0, fn o, acc ->
      acc * 10 + Map.fetch!(mapping, o)
    end)
  end

  defp deduce_mapping(patterns) do
    patterns_by_length =
      patterns
      |> Enum.group_by(&length/1)

    one = hd(patterns_by_length[2])
    four = hd(patterns_by_length[4])
    seven = hd(patterns_by_length[3])
    eight = hd(patterns_by_length[7])
    
    six_segments = patterns_by_length[6]

    nine = Enum.find(six_segments, fn p -> contains_all(p, four) end)
    zero = Enum.find(six_segments, fn p -> p != nine and contains_all(p, one) end)
    six = Enum.find(six_segments, &(&1 != zero and &1 != nine))

    five_segments = patterns_by_length[5]    
    three = Enum.find(five_segments, &contains_all(&1, one))
    five = Enum.find(five_segments, &contains_all(six, &1))    
    two = Enum.find(five_segments, &(&1 != three and &1 != five))
    
    %{
      zero => 0,
      one => 1,
      two => 2,
      three => 3,
      four => 4,
      five => 5,
      six => 6,
      seven => 7,
      eight => 8,
      nine => 9
    }
  end

  defp contains_all(larger, smaller) do
    Enum.all?(smaller, &Enum.member?(larger, &1))
  end
end

JumbledSevenSegment.solve()
