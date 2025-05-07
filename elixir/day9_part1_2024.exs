
defmodule DiskSimulator do
  def run do
    {:ok, line} = File.read("input.txt")
    line = String.trim(line)

    initial_disk = build_initial_disk(line)

    initial_array = :array.from_list(initial_disk)
    final_array = defragment_array(initial_array)
    final_disk = :array.to_list(final_array)

    checksum = calculate_checksum(final_disk)
    IO.puts(checksum)
  end

  defp build_initial_disk(line) do
    String.to_charlist(line)
    |> Enum.reduce({[], 0, true}, fn
      char, {acc, file_id, is_file} when char >= ?0 and char <= ?9 ->
        length = char - ?0
        if is_file do
          elements = List.duplicate(Integer.to_string(file_id), length)
          {elements ++ acc, file_id + 1, !is_file}
        else
          elements = List.duplicate(".", length)
          {elements ++ acc, file_id, !is_file}
        end
    end)
    |> elem(0)
    |> Enum.reverse()
  end

  defp defragment_array(disk_array) do
    case find_defrag_indices(disk_array) do
      {l, r} when l != -1 and r != -1 ->
        r_val = :array.get(r, disk_array)
        disk_array = :array.set(l, r_val, disk_array)
        disk_array = :array.set(r, ".", disk_array)
        defragment_array(disk_array)
      _ ->
        disk_array
    end
  end

  defp find_defrag_indices(disk_array) do
    size = :array.size(disk_array)
    l = find_first_free(disk_array, 0, size)

    if l == -1 do
      {-1, -1}
    else
      r = find_last_file(disk_array, size - 1, l + 1)
      {l, r}
    end
  end

  defp find_first_free(disk_array, i, size) do
    if i < size do
      if :array.get(i, disk_array) == "." do
        i
      else
        find_first_free(disk_array, i + 1, size)
      end
    else
      -1
    end
  end

  defp find_last_file(disk_array, i, e) do
    if i >= e do
      if :array.get(i, disk_array) == "." do
        find_last_file(disk_array, i - 1, e)
      else
        i
      end
    else
      -1
    end
  end

  defp calculate_checksum(disk) do
    disk
    |> Enum.with_index()
    |> Enum.reduce(0, fn
      {".", _}, acc -> acc
      {val, idx}, acc -> acc + idx * String.to_integer(val)
    end)
  end
end

DiskSimulator.run()
