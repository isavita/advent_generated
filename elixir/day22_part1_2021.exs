
defmodule RebootStep do
  defstruct action: nil, x_start: 0, x_end: 0, y_start: 0, y_end: 0, z_start: 0, z_end: 0
end

defmodule Reboot do
  def solve do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_reboot_step/1)
    |> filter_initialization_steps()
    |> execute_reboot_steps()
    |> IO.puts()
  end

  def parse_reboot_step(line) do
    [action, coords] = String.split(line, " ")
    [x, y, z] = String.split(coords, ",")

    %RebootStep{
      action: action,
      x_start: parse_range(x),
      x_end: parse_range(x, :end),
      y_start: parse_range(y),
      y_end: parse_range(y, :end),
      z_start: parse_range(z),
      z_end: parse_range(z, :end)
    }
  end

  def parse_range(range_str, pos \\ :start) do
    range_str 
    |> String.slice(2..-1) 
    |> String.split("..")
    |> then(fn 
      [start, end_val] when pos == :start -> String.to_integer(start)
      [start, end_val] when pos == :end -> String.to_integer(end_val)
    end)
  end

  def filter_initialization_steps(steps) do
    Enum.filter(steps, fn step ->
      step.x_start >= -50 and step.x_end <= 50 and
      step.y_start >= -50 and step.y_end <= 50 and
      step.z_start >= -50 and step.z_end <= 50
    end)
  end

  def execute_reboot_steps(steps) do
    steps
    |> Enum.reduce(MapSet.new(), fn step, cubes ->
      update_cubes(cubes, step)
    end)
    |> MapSet.size()
  end

  def update_cubes(cubes, step) do
    for x <- step.x_start..step.x_end,
        y <- step.y_start..step.y_end,
        z <- step.z_start..step.z_end,
        reduce: cubes do
      acc ->
        case step.action do
          "on" -> MapSet.put(acc, {x, y, z})
          "off" -> MapSet.delete(acc, {x, y, z})
        end
    end
  end
end

Reboot.solve()
