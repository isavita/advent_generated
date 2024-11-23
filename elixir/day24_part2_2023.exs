
defmodule Day24 do
  def solve do
    {:ok, input} = File.read("input.txt")
    
    input
    |> String.trim()
    |> String.split("\n")
    |> solve_input()
    |> IO.puts()
  end

  def solve_input(input) do
    hailstones = parse_input(Enum.take(input, 3))
    
    [s1, s2 | [s0 | _]] = hailstones
    
    ref1 = subtract_hailstone(s1, s0)
    ref2 = subtract_hailstone(s2, s0)
    
    t1 = intersection_time(ref2, ref1)
    t2 = intersection_time(ref1, ref2)
    
    rock1 = add_vec(s1.p, multiply_vec(s1.v, t1))
    rock2 = add_vec(s2.p, multiply_vec(s2.v, t2))
    
    rp = subtract_vec(
      rock1, 
      multiply_vec(
        divide_vec(
          subtract_vec(rock2, rock1), 
          t2 - t1
        ), 
        t1
      )
    )
    
    Enum.reduce([rp.x, rp.y, rp.z], 0, &(&1 + &2))
    |> trunc()
  end

  def parse_input(input) do
    input
    |> Enum.map(fn line ->
      numbers = Regex.scan(~r/-?\d+/, line)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)
      
      %{
        p: %{
          x: Enum.at(numbers, 0),
          y: Enum.at(numbers, 1), 
          z: Enum.at(numbers, 2)
        },
        v: %{
          x: Enum.at(numbers, 3),
          y: Enum.at(numbers, 4),
          z: Enum.at(numbers, 5)
        }
      }
    end)
  end

  def subtract_hailstone(stone1, stone2) do
    %{
      p: subtract_vec(stone1.p, stone2.p),
      v: subtract_vec(stone1.v, stone2.v)
    }
  end

  def subtract_vec(v1, v2) do
    %{
      x: v1.x - v2.x,
      y: v1.y - v2.y,
      z: v1.z - v2.z
    }
  end

  def add_vec(v1, v2) do
    %{
      x: v1.x + v2.x,
      y: v1.y + v2.y,
      z: v1.z + v2.z
    }
  end

  def multiply_vec(v, scalar) do
    %{
      x: v.x * scalar,
      y: v.y * scalar,
      z: v.z * scalar
    }
  end

  def divide_vec(v, scalar) do
    %{
      x: v.x / scalar,
      y: v.y / scalar,
      z: v.z / scalar
    }
  end

  def cross_product(v1, v2) do
    %{
      x: v1.y * v2.z - v1.z * v2.y,
      y: v1.z * v2.x - v1.x * v2.z,
      z: v1.x * v2.y - v1.y * v2.x
    }
  end

  def dot_product(v1, v2) do
    v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
  end

  def intersection_time(r, s) do
    plane = cross_product(r.p, add_vec(r.p, r.v))
    -(dot_product(s.p, plane) / dot_product(s.v, plane))
  end
end

Day24.solve()
