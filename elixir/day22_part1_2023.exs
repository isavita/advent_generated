
defmodule Day22 do
  def solve do
    read_input()
    |> parse_input()
    |> settle()
    |> count_disintegratable_bricks()
  end

  def read_input do
    File.read!("input.txt")
    |> String.trim()
    |> String.split("\n")
  end

  def parse_input(input) do
    input
    |> Enum.map(fn line ->
      [x1, y1, z1, x2, y2, z2] = 
        line 
        |> String.split(["~", ","])
        |> Enum.map(&String.to_integer/1)
      
      %{
        mini: %{x: min(x1, x2), y: min(y1, y2), z: min(z1, z2)},
        maxi: %{x: max(x1, x2), y: max(y1, y2), z: max(z1, z2)},
        based_on: [],
        support: []
      }
    end)
    |> Enum.sort_by(& &1.maxi.z)
  end

  def settle(bricks) do
    bricks
    |> Enum.reduce([], fn brick, settled ->
      {new_brick, settled} = place_brick(brick, settled)
      [new_brick | settled]
    end)
    |> Enum.reverse()
  end

  def place_brick(brick, settled) do
    support_z = 
      settled
      |> Enum.filter(fn s -> 
        intersects_xy?(brick, s)
      end)
      |> Enum.map(& &1.maxi.z)
      |> Enum.max(fn -> 0 end)

    based_on = 
      settled
      |> Enum.filter(fn s -> 
        intersects_xy?(brick, s) and s.maxi.z == support_z
      end)

    delta_z = brick.maxi.z - brick.mini.z
    new_brick = %{
      brick | 
      mini: %{brick.mini | z: support_z + 1},
      maxi: %{brick.maxi | z: support_z + 1 + delta_z},
      based_on: based_on
    }

    updated_settled = 
      settled
      |> Enum.map(fn s -> 
        if s in based_on do
          %{s | support: [new_brick | s.support]}
        else
          s
        end
      end)

    {new_brick, updated_settled}
  end

  def intersects_xy?(a, b) do
    max(a.mini.x, b.mini.x) <= min(a.maxi.x, b.maxi.x) and
    max(a.mini.y, b.mini.y) <= min(a.maxi.y, b.maxi.y)
  end

  def count_disintegratable_bricks(bricks) do
    bricks
    |> Enum.count(fn brick ->
      brick.support
      |> Enum.all?(fn supported -> 
        length(supported.based_on) > 1 
      end)
    end)
  end
end

# Run the solution
IO.puts(Day22.solve())
