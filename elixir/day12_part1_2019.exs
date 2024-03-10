defmodule Day12 do
  def call do
    input = File.read!("input.txt") |> String.trim() |> parse_input()
    final_state = Enum.reduce(1..1000, input, fn _step, state ->
      apply_gravity(state) |> apply_velocity()
    end)
    total_energy(final_state)
  end

  defp parse_input(input) do
    input
    |> String.split("\n")
    |> Enum.map(&parse_moon/1)
  end

  defp parse_moon(line) do
    x = Regex.run(~r/x=(-?\d+)/, line, capture: :all_but_first) |> hd() |> String.to_integer()
    y = Regex.run(~r/y=(-?\d+)/, line, capture: :all_but_first) |> hd() |> String.to_integer()
    z = Regex.run(~r/z=(-?\d+)/, line, capture: :all_but_first) |> hd() |> String.to_integer()
    {x, y, z, 0, 0, 0}
  end

  defp apply_gravity(state) do
    Enum.reduce(state, state, fn moon, acc ->
      {x, y, z, vx, vy, vz} = moon
      new_vel = Enum.reduce(acc, {0, 0, 0}, fn {ox, oy, oz, _, _, _}, {inc_vx, inc_vy, inc_vz} ->
        inc_vx = inc_vx + calc_velocity_change(x, ox)
        inc_vy = inc_vy + calc_velocity_change(y, oy)
        inc_vz = inc_vz + calc_velocity_change(z, oz)
        {inc_vx, inc_vy, inc_vz}
      end)
      List.delete(acc, moon) ++ [{x, y, z, vx + elem(new_vel, 0), vy + elem(new_vel, 1), vz + elem(new_vel, 2)}]
    end)
  end

  defp calc_velocity_change(a, b) do
    cond do
      a < b -> 1
      a > b -> -1
      true -> 0
    end
  end

  defp apply_velocity(state) do
    Enum.map(state, fn {x, y, z, vx, vy, vz} ->
      {x + vx, y + vy, z + vz, vx, vy, vz}
    end)
  end

  defp total_energy(state) do
    Enum.reduce(state, 0, fn {x, y, z, vx, vy, vz}, acc ->
      potential = abs(x) + abs(y) + abs(z)
      kinetic = abs(vx) + abs(vy) + abs(vz)
      acc + (potential * kinetic)
    end)
  end
end

Day12.call() |> IO.puts()