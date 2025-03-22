
defmodule Elf do
  defstruct pos: {0, 0}, moving: false, next_pos: nil

  def around_all_empty?(%{pos: {x, y}}, map, dirs) do
    Enum.all?(dirs, fn {dx, dy} ->
      not Map.has_key?(map, {x + dx, y + dy})
    end)
  end

  def elf_in_direction?(%{pos: {x, y}}, wanna_go, map, dirs) do
    Enum.any?([-1, 0, 1], fn j ->
      {dx, dy} = Enum.at(dirs, rem(wanna_go + j, 8))
      Map.has_key?(map, {x + dx, y + dy})
    end)
  end
end

defmodule Day23 do
  def parse do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, row} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _col} -> char == "#" end)
      |> Enum.map(fn {_char, col} ->
        pos = {row, col}
        {pos, %Elf{pos: pos}}
      end)
    end)
    |> Map.new()
    |> then(fn map -> {Map.values(map), map} end)
  end

  def run(elves, map, order, curr_dir, dirs) do
    {proposes, elves} =
      Enum.reduce(elves, {%{}, []}, fn elf, {proposes_acc, elves_acc} ->
        if Elf.around_all_empty?(elf, map, dirs) do
          {proposes_acc, [elf | elves_acc]}
        else
          case find_next_pos(elf, map, order, curr_dir, dirs) do
            nil ->
              {proposes_acc, [elf | elves_acc]}

            next_pos ->
              proposes =
                Map.update(proposes_acc, next_pos, 1, fn count -> count + 1 end)

              updated_elf = %{elf | moving: true, next_pos: next_pos}
              {proposes, [updated_elf | elves_acc]}
          end
        end
      end)

    {someone_moved, new_elves, new_map} =
      Enum.reduce(elves, {false, [], map}, fn elf,
                                                 {someone_moved_acc, elves_acc, map_acc} ->
        if not elf.moving do
          {someone_moved_acc, [elf | elves_acc], map_acc}
        else
          if Map.get(proposes, elf.next_pos, 0) > 1 do
            updated_elf = %{elf | moving: false}
            {someone_moved_acc, [updated_elf | elves_acc], map_acc}
          else
            someone_moved = true
            new_map = Map.delete(map_acc, elf.pos) |> Map.put(elf.next_pos, elf)
            updated_elf = %{elf | pos: elf.next_pos, moving: false}

            {someone_moved, [updated_elf | elves_acc], new_map}
          end
        end
      end)

    {someone_moved, new_elves, new_map}
  end

  def find_next_pos(elf, map, order, curr_dir, dirs) do
    Enum.find_value(0..3, nil, fn i ->
      dir = Enum.at(order, rem(curr_dir + i, 4))

      if Elf.elf_in_direction?(elf, dir, map, dirs) do
        nil
      else
        {dx, dy} = Enum.at(dirs, dir)
        next_pos = {elem(elf.pos, 0) + dx, elem(elf.pos, 1) + dy}
        next_pos
      end
    end)
  end

  def main do
    dirs =
      [
        {-1, -1},
        {-1, 0},
        {-1, 1},
        {0, 1},
        {1, 1},
        {1, 0},
        {1, -1},
        {0, -1}
      ]

    order = [1, 5, 7, 3]
    curr_dir = 0

    {elves, map} = parse()

    do_run(elves, map, order, curr_dir, dirs, 0)
  end

  def do_run(elves, map, order, curr_dir, dirs, i) do
    {someone_moved, new_elves, new_map} = run(elves, map, order, curr_dir, dirs)

    if not someone_moved do
      IO.puts(i + 1)
    else
      new_curr_dir = rem(curr_dir + 1, 4)
      do_run(new_elves, new_map, order, new_curr_dir, dirs, i + 1)
    end
  end
end

Day23.main()
