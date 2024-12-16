
defmodule Day13 do
  def solve do
    {:ok, input} = File.read("input.txt")
    {track, carts} = parse_input(input)
    find_collision(track, carts)
  end

  defp parse_input(input) do
    lines = String.split(input, "\n", trim: true)
    track = Enum.with_index(lines) |> Enum.map(fn {line, y} ->
      String.graphemes(line) |> Enum.with_index() |> Enum.map(fn {char, x} ->
        case char do
          ">" -> {x, y, "-", :right}
          "<" -> {x, y, "-", :left}
          "^" -> {x, y, "|", :up}
          "v" -> {x, y, "|", :down}
          _ -> {x, y, char, nil}
        end
      end)
    end)
    {
      Enum.map(track, fn row -> Enum.map(row, fn {_, _, char, _} -> char end) end),
      Enum.filter(Enum.flat_map(track, fn row -> row end), fn {_, _, _, dir} -> dir != nil end)
      |> Enum.map(fn {x, y, _, dir} -> %{x: x, y: y, dir: dir, turn: 0} end)
    }
  end

  defp find_collision(track, carts) do
    loop(track, carts)
  end

  defp loop(track, carts) do
    new_carts =
      carts
      |> Enum.sort_by(fn cart -> {cart.y, cart.x} end)
      |> Enum.map(fn cart -> move_cart(track, cart) end)

    case find_first_collision(new_carts) do
      nil -> loop(track, new_carts)
      {x, y} -> IO.puts("#{x},#{y}")
    end
  end

  defp move_cart(track, cart) do
    case cart.dir do
      :right -> move_right(track, cart)
      :left -> move_left(track, cart)
      :up -> move_up(track, cart)
      :down -> move_down(track, cart)
    end
  end

  defp move_down(track, %{x: x, y: y, dir: _dir, turn: turn} = cart) do
    case Enum.at(Enum.at(track, y + 1), x) do
      "/" -> %{cart | dir: :left, y: y + 1}
      "\\" -> %{cart | dir: :right, y: y + 1}
      "+" ->
        case turn do
          0 -> %{cart | dir: :right, turn: 1, y: y + 1}
          1 -> %{cart | turn: 2, y: y + 1}
          2 -> %{cart | dir: :left, turn: 0, y: y + 1}
        end
      "|" -> %{cart | y: y + 1}
    end
  end

  defp move_up(track, %{x: x, y: y, dir: _dir, turn: turn} = cart) do
    case Enum.at(Enum.at(track, y - 1), x) do
      "/" -> %{cart | dir: :right, y: y - 1}
      "\\" -> %{cart | dir: :left, y: y - 1}
      "+" ->
        case turn do
          0 -> %{cart | dir: :left, turn: 1, y: y - 1}
          1 -> %{cart | turn: 2, y: y - 1}
          2 -> %{cart | dir: :right, turn: 0, y: y - 1}
        end
      "|" -> %{cart | y: y - 1}
    end
  end

  defp move_left(track, %{x: x, y: y, dir: _dir, turn: turn} = cart) do
    case Enum.at(Enum.at(track, y), x - 1) do
      "/" -> %{cart | dir: :down, x: x - 1}
      "\\" -> %{cart | dir: :up, x: x - 1}
      "+" ->
        case turn do
          0 -> %{cart | dir: :down, turn: 1, x: x - 1}
          1 -> %{cart | turn: 2, x: x - 1}
          2 -> %{cart | dir: :up, turn: 0, x: x - 1}
        end
      "-" -> %{cart | x: x - 1}
    end
  end

  defp move_right(track, %{x: x, y: y, dir: _dir, turn: turn} = cart) do
    case Enum.at(Enum.at(track, y), x + 1) do
      "\\" -> %{cart | dir: :down, x: x + 1}
      "/" -> %{cart | dir: :up, x: x + 1}
      "+" ->
        case turn do
          0 -> %{cart | dir: :up, turn: 1, x: x + 1}
          1 -> %{cart | turn: 2, x: x + 1}
          2 -> %{cart | dir: :down, turn: 0, x: x + 1}
        end
      "-" -> %{cart | x: x + 1}
    end
  end

  defp find_first_collision(carts) do
    carts
    |> Enum.with_index()
    |> Enum.reduce_while(nil, fn {cart1, i}, acc ->
      collision =
        Enum.slice(carts, i + 1..-1)
        |> Enum.find(fn cart2 -> cart1.x == cart2.x and cart1.y == cart2.y end)

      if collision do
        {:halt, {cart1.x, cart1.y}}
      else
        {:cont, acc}
      end
    end)
  end
end

Day13.solve()
