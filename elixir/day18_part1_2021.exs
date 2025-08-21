defmodule Snailfish do
  def main do
    case File.read("input.txt") do
      {:ok, content} ->
        lines = String.split(content, ~r/\r?\n/, trim: true)
        if lines == [] do
          IO.puts "No snailfish numbers found in the file."
        else
          numbers = Enum.map(lines, &parse_snail/1)
          result = Enum.reduce(tl(numbers), hd(numbers), fn n, acc -> add(acc, n) end)
          IO.puts magnitude(result)
        end
      {:error, reason} ->
        IO.puts "Error opening input file: #{reason}"
    end
  end

  def parse_snail(str) do
    str = String.trim(str)
    if String.first(str) != "[" do
      {:regular, String.to_integer(str)}
    else
      inner = String.slice(str, 1, byte_size(str) - 2)
      {left_str, right_str} = split_at_comma(inner)
      {:pair, parse_snail(left_str), parse_snail(right_str)}
    end
  end

  def split_at_comma(str) do
    split_at_comma(str, 0, "")
  end

  def split_at_comma(<<>>, _depth, acc), do: {acc, ""}

  def split_at_comma(<<c, rest::binary>>, depth, acc) when c == ?[ do
    split_at_comma(rest, depth + 1, acc <> <<c>>)
  end

  def split_at_comma(<<c, rest::binary>>, depth, acc) when c == ?] do
    split_at_comma(rest, depth - 1, acc <> <<c>>)
  end

  def split_at_comma(<<c, rest::binary>>, depth, acc) when c == ?, and depth == 0 do
    {acc, rest}
  end

  def split_at_comma(<<c, rest::binary>>, depth, acc) do
    split_at_comma(rest, depth, acc <> <<c>>)
  end

  def add(a, b), do: reduce({:pair, a, b})

  def reduce(sn) do
    case explode(sn, 0) do
      {:exploded, sn, _, _} -> reduce(sn)
      :no_explode ->
        case split(sn) do
          {:split, sn} -> reduce(sn)
          :no_split -> sn
        end
    end
  end

  def explode({:regular, _} = sn, _depth), do: :no_explode

  def explode({:pair, left, right}, depth) when depth == 4 do
    {:exploded, {:regular, 0}, get_value(left), get_value(right)}
  end

  def explode({:pair, left, right}, depth) do
    case explode(left, depth + 1) do
      {:exploded, new_left, l_val, r_val} ->
        new_right = if r_val > 0, do: add_left(right, r_val), else: right
        {:exploded, {:pair, new_left, new_right}, l_val, 0}
      :no_explode ->
        case explode(right, depth + 1) do
          {:exploded, new_right, l_val, r_val} ->
            new_left = if l_val > 0, do: add_right(left, l_val), else: left
            {:exploded, {:pair, new_left, new_right}, 0, r_val}
          :no_explode ->
            :no_explode
        end
    end
  end

  def split({:regular, v}) when v >= 10 do
    left = div(v, 2)
    right = div(v + 1, 2)
    {:split, {:pair, {:regular, left}, {:regular, right}}}
  end

  def split({:regular, _}), do: :no_split

  def split({:pair, left, right}) do
    case split(left) do
      {:split, new_left} -> {:split, {:pair, new_left, right}}
      :no_split ->
        case split(right) do
          {:split, new_right} -> {:split, {:pair, left, new_right}}
          :no_split -> :no_split
        end
    end
  end

  def magnitude({:regular, v}), do: v
  def magnitude({:pair, left, right}), do: 3 * magnitude(left) + 2 * magnitude(right)

  def add_left({:regular, v}, val), do: {:regular, v + val}
  def add_left({:pair, left, right}, val), do: {:pair, add_left(left, val), right}

  def add_right({:regular, v}, val), do: {:regular, v + val}
  def add_right({:pair, left, right}, val), do: {:pair, left, add_right(right, val)}

  def get_value({:regular, v}), do: v
end

Snailfish.main()