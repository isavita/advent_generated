
defmodule Particle do
  defstruct p: {0, 0, 0}, v: {0, 0, 0}, a: {0, 0, 0}
end

defmodule Main do
  def run do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_particle/1)
    |> Enum.with_index()
    |> Enum.reduce({nil, {1_000_000, 1_000_000, 1_000_000}}, &find_closest/2)
    |> elem(0)
    |> IO.puts()
  end

  defp parse_particle(line) do
    [p, v, a] = String.split(line, ", ")
    %Particle{
      p: parse_coords(p),
      v: parse_coords(v),
      a: parse_coords(a)
    }
  end

  defp parse_coords(coord) do
    coord
    |> String.slice(3..-2)
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  defp find_closest({particle, index}, {closest_index, closest}) do
    {accel, velocity, position} = {manhattan(particle.a), manhattan(particle.v), manhattan(particle.p)}
    if closest_index == nil or 
       compare_particles({accel, velocity, position}, closest) do
      {index, {accel, velocity, position}}
    else
      {closest_index, closest}
    end
  end

  defp manhattan({x, y, z}), do: abs(x) + abs(y) + abs(z)

  defp compare_particles({a1, v1, p1}, {a2, v2, p2}) do
    a1 < a2 or (a1 == a2 and (v1 < v2 or (v1 == v2 and p1 < p2)))
  end
end

Main.run()
