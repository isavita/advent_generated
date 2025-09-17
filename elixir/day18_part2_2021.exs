defmodule SnailNum do
  defstruct value: nil, left: nil, right: nil

  def parse(s) do
    s = String.trim(s)
    if String.starts_with?(s, "[") do
      inner = String.slice(s, 1, String.length(s) - 2)
      idx = find_split_index(String.to_charlist(inner), 0, 0)
      left_s = String.slice(inner, 0, idx)
      right_s = String.slice(inner, idx + 1, String.length(inner) - idx - 1)
      left = SnailNum.parse(left_s)
      right = SnailNum.parse(right_s)
      %SnailNum{value: 0, left: left, right: right}
    else
      %SnailNum{value: String.to_integer(s)}
    end
  end

  def add(a, b) do
    reduce(%SnailNum{value: 0, left: a, right: b})
  end

  def reduce(node) do
    reduce_loop(node)
  end

  defp reduce_loop(node) do
    {node1, _l, _r, exploded} = explode(node, 0)
    if exploded do
      reduce_loop(node1)
    else
      {node2, split} = split(node1)
      if split do
        reduce_loop(node2)
      else
        node1
      end
    end
  end

  def explode(node, depth) do
    if regular?(node) do
      {node, 0, 0, false}
    else
      if depth == 4 do
        lv = node.left.value
        rv = node.right.value
        {%SnailNum{value: 0}, lv, rv, true}
      else
        case explode(node.left, depth + 1) do
          {new_left, lval, rval, true} ->
            updated_right = if rval > 0 and node.right != nil, do: add_leftmost(node.right, rval), else: node.right
            new_node = %SnailNum{node | left: new_left, right: updated_right}
            {new_node, lval, 0, true}
          {_, _, _, false} ->
            case explode(node.right, depth + 1) do
              {new_right, lval2, rval2, true} ->
                updated_left = if lval2 > 0 and node.left != nil, do: add_rightmost(node.left, lval2), else: node.left
                new_node = %SnailNum{node | left: updated_left, right: new_right}
                {new_node, 0, rval2, true}
              {_, _, _, false} ->
                {node, 0, 0, false}
            end
        end
      end
    end
  end

  def add_leftmost(node, val) when val > 0 do
    if regular?(node) do
      %SnailNum{node | value: node.value + val}
    else
      left = add_leftmost(node.left, val)
      %SnailNum{node | left: left}
    end
  end
  def add_leftmost(node, _val), do: node

  def add_rightmost(node, val) when val > 0 do
    if regular?(node) do
      %SnailNum{node | value: node.value + val}
    else
      right = add_rightmost(node.right, val)
      %SnailNum{node | right: right}
    end
  end
  def add_rightmost(node, _val), do: node

  def split(node) do
    if regular?(node) do
      if node.value >= 10 do
        left = %SnailNum{value: div(node.value, 2)}
        right = %SnailNum{value: div(node.value + 1, 2)}
        new_node = %SnailNum{value: 0, left: left, right: right}
        {new_node, true}
      else
        {node, false}
      end
    else
      case split(node.left) do
        {new_left, true} -> {%SnailNum{node | left: new_left}, true}
        {_, false} ->
          case split(node.right) do
            {new_right, true} -> {%SnailNum{node | right: new_right}, true}
            {_, false} -> {node, false}
          end
      end
    end
  end

  def magnitude(node) do
    if regular?(node) do
      node.value
    else
      3 * magnitude(node.left) + 2 * magnitude(node.right)
    end
  end

  def regular?(%SnailNum{left: nil, right: nil}) do true end
  def regular?(_), do: false

  defp find_split_index(chars, i, balance) do
    if i >= length(chars) do
      0
    else
      ch = Enum.at(chars, i)
      cond do
        ch == ?[ -> find_split_index(chars, i + 1, balance + 1)
        ch == ?] -> find_split_index(chars, i + 1, balance - 1)
        ch == ?, and balance == 0 -> i
        true -> find_split_index(chars, i + 1, balance)
      end
    end
  end
end

defmodule Main do
  def main do
    case File.read("input.txt") do
      {:ok, content} ->
        lines = content |> String.split(["\n", "\r\n"], trim: true)
        nums = Enum.map(lines, &SnailNum.parse/1)
        if Enum.empty?(nums) do
          IO.puts("No snailfish numbers found in the file.")
        else
          max_mag = max_magnitude(nums)
          IO.puts(Integer.to_string(max_mag))
        end
      {:error, _} ->
        IO.puts("Unable to open file")
        System.halt(1)
    end
  end

  def max_magnitude(nums) do
    n = length(nums)
    Enum.reduce(0..(n - 1), 0, fn i, acc ->
      Enum.reduce(0..(n - 1), acc, fn j, acc2 ->
        if i != j do
          a = Enum.at(nums, i)
          b = Enum.at(nums, j)
          s1 = SnailNum.magnitude(SnailNum.add(a, b))
          s2 = SnailNum.magnitude(SnailNum.add(b, a))
          m = if s1 > s2, do: s1, else: s2
          if m > acc2, do: m, else: acc2
        else
          acc2
        end
      end)
    end)
  end
end

Main.main()