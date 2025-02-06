
defmodule Solution do
  def parse_input(path) do
    File.stream!(path)
    |> Enum.map(fn line ->
      [_, x, y, z] = Regex.run(~r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>", line)
      {String.to_integer(x), String.to_integer(y), String.to_integer(z)}
    end)
  end

  def apply_gravity(moons, axis) do
    Enum.map(moons, fn {pos1, vel1} = moon1 ->
      vel =
        Enum.reduce(moons, vel1, fn {pos2, _}, acc_vel ->
          cond do
            elem(pos1, axis) > elem(pos2, axis) ->
              put_elem(acc_vel, axis, elem(acc_vel, axis) - 1)

            elem(pos1, axis) < elem(pos2, axis) ->
              put_elem(acc_vel, axis, elem(acc_vel, axis) + 1)

            true ->
              acc_vel
          end
        end)

      {pos1, vel}
    end)
  end

  def apply_velocity(moons, axis) do
    Enum.map(moons, fn {pos, vel} ->
      {put_elem(pos, axis, elem(pos, axis) + elem(vel, axis)), vel}
    end)
  end

  def find_cycle(moons, initial_moons, axis) do
    Stream.iterate(1, &(&1 + 1))
    |> Enum.reduce_while({moons, 1}, fn _, {current_moons, steps} ->
      new_moons =
        current_moons
        |> apply_gravity(axis)
        |> apply_velocity(axis)

      match =
        Enum.all?(Enum.zip(new_moons, initial_moons), fn {{pos, vel}, {initial_pos, initial_vel}} ->
          elem(pos, axis) == elem(initial_pos, axis) and elem(vel, axis) == elem(initial_vel, axis)
        end)

      if match do
        {:halt, steps}
      else
        {:cont, {new_moons, steps + 1}}
      end
    end)
  end

  def lcm(a, b) do
    a * b |> div(gcd(a, b))
  end

  def gcd(a, 0), do: a
  def gcd(a, b), do: gcd(b, rem(a, b))

  def solve(path) do
    initial_moons =
      parse_input(path)
      |> Enum.map(fn pos -> {pos, {0, 0, 0}} end)

    moons = initial_moons

    cycle_x = find_cycle(moons, initial_moons, 0)
    cycle_y = find_cycle(moons, initial_moons, 1)
    cycle_z = find_cycle(moons, initial_moons, 2)

    lcm(cycle_x, lcm(cycle_y, cycle_z))
  end
end

Solution.solve("input.txt") |> IO.puts()
