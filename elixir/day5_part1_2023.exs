
defmodule Almanac do
  def solve do
    {:ok, content} = File.read("input.txt")
    
    [seeds_line | map_lines] = String.split(content, "\n", trim: true)
    
    seeds = 
      seeds_line 
      |> String.slice(7..-1) 
      |> String.split(" ") 
      |> Enum.map(&String.to_integer/1)
    
    maps = 
      map_lines
      |> Enum.chunk_by(fn line -> String.contains?(line, "map:") end)
      |> Enum.filter(fn chunk -> !String.contains?(Enum.at(chunk, 0), "map:") end)
      |> Enum.map(fn chunk -> 
        chunk
        |> Enum.map(fn line -> 
          line 
          |> String.split(" ") 
          |> Enum.map(&String.to_integer/1)
        end)
        |> Enum.map(fn [dest, src, len] -> {src, dest, len} end)
      end)
    
    seeds
    |> Enum.map(fn seed -> 
      Enum.reduce(maps, seed, fn map, current -> convert_number(current, map) end)
    end)
    |> Enum.min()
    |> IO.puts()
  end
  
  defp convert_number(number, ranges) do
    Enum.find_value(ranges, number, fn {src_start, dest_start, length} ->
      if number >= src_start and number < src_start + length do
        dest_start + (number - src_start)
      else
        nil
      end
    end)
  end
end

Almanac.solve()
