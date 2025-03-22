
defmodule LensLibrary do
  def hash(string) do
    String.codepoints(string)
    |> Enum.reduce(0, fn char, acc ->
      acc
      |> Kernel.+(String.to_charlist(char) |> hd())
      |> Kernel.*(17)
      |> rem(256)
    end)
  end

  def part1(input_file) do
    File.read!(input_file)
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&hash/1)
    |> Enum.sum()
  end

  def part2(input_file) do
    steps =
      File.read!(input_file)
      |> String.trim()
      |> String.split(",")

    boxes = Enum.reduce(steps, Enum.to_list(0..255) |> Enum.map(fn n -> {n, []} end) |> Map.new(), fn step, acc ->
      case String.contains?(step, "=") do
        true ->
          [label, focal_length] = String.split(step, "=")
          focal_length = String.to_integer(focal_length)
          box_number = hash(label)
          box = Map.get(acc, box_number)

          case Enum.find_index(box, fn {l, _} -> l == label end) do
            nil ->
              Map.put(acc, box_number, box ++ [{label, focal_length}])

            index ->
              {_, old_focal_length} = Enum.at(box, index)
              updated_box = List.replace_at(box, index, {label, focal_length})
              Map.put(acc, box_number, updated_box)
          end

        false ->
          label = String.replace(step, "-", "")
          box_number = hash(label)
          box = Map.get(acc, box_number)
          updated_box = Enum.reject(box, fn {l, _} -> l == label end)
          Map.put(acc, box_number, updated_box)
      end
    end)

    boxes
    |> Enum.map(fn {box_number, lenses} ->
      Enum.with_index(lenses, 1)
      |> Enum.map(fn {{_, focal_length}, slot_number} ->
        (box_number + 1) * slot_number * focal_length
      end)
      |> Enum.sum()
    end)
    |> Enum.sum()
  end
end

# Main entry point
input_file = "input.txt"
IO.puts("Part 1: #{LensLibrary.part1(input_file)}")
IO.puts("Part 2: #{LensLibrary.part2(input_file)}")
