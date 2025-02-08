
defmodule Solver do
  def solve(filename \\ "input.txt") do
    {dirs, _} =
      filename
      |> File.stream!()
      |> Stream.map(&String.trim/1)
      |> Enum.reduce({%{"/" => 0}, ["/"]}, fn line, {dirs, path} ->
        process_line(line, dirs, path)
      end)

    total_space = 70_000_000
    needed_space = 30_000_000
    used_space = dirs["/"]
    available_space = total_space - used_space
    to_delete = needed_space - available_space

    dirs
    |> Map.values()
    |> Enum.sort()
    |> Enum.find(&(&1 >= to_delete))
    |> IO.puts()
  end

  defp process_line(line, dirs, path) do
    case String.split(line) do
      ["$", "cd", "/"] ->
        {dirs, ["/"]}

      ["$", "cd", ".."] ->
        {dirs, Enum.drop(path, -1)}

      ["$", "cd", dir] ->
        new_path = path ++ [dir]
        {Map.put(dirs, Enum.join(new_path, "/"), 0), new_path}

      ["$", "ls"] ->
        {dirs, path}

      ["dir", _] ->
        {dirs, path}

      [size, _] ->
        size = String.to_integer(size)

        updated_dirs =
          for i <- 1..length(path), reduce: dirs do
            acc ->
              dir_path = Enum.join(Enum.take(path, i), "/")
              Map.update(acc, dir_path, size, &(&1 + size))
          end

        {updated_dirs, path}
    end
  end
end

Solver.solve()
