
defmodule Solution do
  def main() do
    file_content = File.read!("input.txt")
    lines = String.split(file_content, "\n", trim: true)

    initial_state = %{fs: %{"/" => %{}}, path: ["/"]}

    %{fs: root} = Enum.reduce(lines, initial_state, &process_line/2)

    {_total_root_size, all_dir_sizes} = get_directory_sizes(root["/"])

    sum_of_small_dirs =
      all_dir_sizes
      |> Enum.filter(&(&1 <= 100000))
      |> Enum.sum()

    IO.puts(sum_of_small_dirs)
  end

  defp process_line("$ cd /", %{fs: fs}) do
    %{fs: fs, path: ["/"]}
  end

  defp process_line("$ cd ..", %{fs: fs, path: path}) do
    %{fs: fs, path: Enum.slice(path, 0, length(path) - 1)}
  end

  defp process_line("$ cd " <> rest, %{fs: fs, path: path}) do
    dir_name = String.trim(rest)
    %{fs: fs, path: path ++ [dir_name]}
  end

  defp process_line("$ ls", state), do: state

  defp process_line("dir " <> rest, %{fs: fs, path: path}) do
    dir_name = String.trim(rest)
    new_fs = update_in(fs, path ++ [dir_name], fn existing -> existing || %{} end)
    %{fs: new_fs, path: path}
  end

  defp process_line(line, %{fs: fs, path: path}) do
    [size_str, file_name] = String.split(line, " ", parts: 2)
    size = String.to_integer(size_str)
    new_fs = update_in(fs, path ++ [file_name], fn _ -> size end)
    %{fs: new_fs, path: path}
  end

  defp get_directory_sizes(node) when is_integer(node) do
    {node, []}
  end

  defp get_directory_sizes(node) when is_map(node) do
    total_size = 0
    all_subdir_sizes = []

    {final_size, final_collected_sizes} =
      Enum.reduce(node, {total_size, all_subdir_sizes}, fn {_name, child_node}, {current_size, current_collected_sizes} ->
        {child_size, child_dir_sizes} = get_directory_sizes(child_node)
        {current_size + child_size, current_collected_sizes ++ child_dir_sizes}
      end)

    {final_size, [final_size | final_collected_sizes]}
  end
end

Solution.main()
