
defmodule Particle do
  defstruct p: {0, 0, 0}, v: {0, 0, 0}, a: {0, 0, 0}
end

defmodule Main do
  def run do
    particles = parse_input("input.txt")
    particles
    |> simulate(1000)
    |> length()
    |> IO.puts()
  end

  defp parse_input(file) do
    File.read!(file)
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_particle/1)
  end

  defp parse_particle(line) do
    [p, v, a] = String.split(line, ", ")
    %Particle{
      p: parse_coords(p),
      v: parse_coords(v),
      a: parse_coords(a)
    }
  end

  defp parse_coords(coord_str) do
    coord_str
    |> String.trim_leading("p=<")
    |> String.trim_leading("v=<")
    |> String.trim_leading("a=<")
    |> String.trim_trailing(">")
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  defp simulate(particles, ticks) do
    Enum.reduce(1..ticks, particles, fn _, ps ->
      ps
      |> Enum.reduce(%{}, fn particle, acc ->
        new_particle = update_particle(particle)
        pos = Tuple.to_list(new_particle.p) |> Enum.join(",")
        Map.update(acc, pos, [new_particle], &[new_particle | &1])
      end)
      |> Enum.filter(fn {_pos, ps} -> length(ps) == 1 end)
      |> Enum.flat_map(fn {_pos, [p]} -> [p] end)
    end)
  end

  defp update_particle(%Particle{p: {px, py, pz}, v: {vx, vy, vz}, a: {ax, ay, az}}) do
    new_v = {vx + ax, vy + ay, vz + az}
    new_p = {px + elem(new_v, 0), py + elem(new_v, 1), pz + elem(new_v, 2)}
    %Particle{p: new_p, v: new_v, a: {ax, ay, az}}
  end
end

Main.run()
