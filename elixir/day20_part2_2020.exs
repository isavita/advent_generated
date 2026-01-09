
defmodule Solution do
  @monster [
    {0, 18}, {1, 0}, {1, 5}, {1, 6}, {1, 11}, {1, 12}, {1, 17}, {1, 18}, {1, 19},
    {2, 1}, {2, 4}, {2, 7}, {2, 10}, {2, 13}, {2, 16}
  ]

  def main do
    input = File.read!("input.txt") |> String.trim()
    tiles = parse(input)
    size = round(:math.sqrt(length(tiles)))
    placed = try do backtrack(%{}, tiles, size, 0) catch res -> res end
    image = assemble(placed, size)
    total = List.flatten(image) |> Enum.count(&(&1 == "#"))
    pixels = orientations(image) |> Enum.find_value(fn img ->
      m = find_monsters(img)
      if length(m) > 0, do: length(m)
    end) || 0
    IO.puts(total - pixels)
  end

  def parse(input) do
    String.split(input, "\n\n", trim: true) |> Enum.map(fn block ->
      [h | r] = String.split(block, "\n", trim: true)
      [id] = Regex.run(~r/\d+/, h)
      grid = Enum.map(r, &String.graphemes/1)
      {String.to_integer(id), orientations(grid)}
    end)
  end

  def orientations(grid) do
    r1 = rotate(grid); r2 = rotate(r1); r3 = rotate(r2)
    [grid, r1, r2, r3] |> Enum.flat_map(&[&1, flip(&1)]) |> Enum.uniq()
  end

  def rotate(g), do: g |> List.zip() |> Enum.map(&(Tuple.to_list(&1) |> Enum.reverse()))
  def flip(g), do: Enum.map(g, &Enum.reverse/1)

  def backtrack(p, [], _, _), do: throw(p)
  def backtrack(p, rem, s, pos) do
    r = div(pos, s); c = rem(pos, s)
    for {id, gs} <- rem, g <- gs, matches?(p, g, r, c) do
      backtrack(Map.put(p, {r, c}, g), List.keydelete(rem, id, 0), s, pos + 1)
    end
  end

  def matches?(p, g, r, c) do
    (r == 0 || List.last(p[{r-1, c}]) == List.first(g)) and
    (c == 0 || Enum.map(p[{r, c-1}], &List.last/1) == Enum.map(g, &List.first/1))
  end

  def assemble(p, s) do
    for r <- 0..(s-1) do
      tiles = for c <- 0..(s-1), do: p[{r, c}]
      tiles
      |> Enum.map(fn t -> Enum.slice(t, 1..-2) |> Enum.map(&Enum.slice(&1, 1..-2)) end)
      |> List.zip()
      |> Enum.map(&(Tuple.to_list(&1) |> List.flatten()))
    end |> Enum.concat()
  end

  def find_monsters(img) do
    h = length(img); w = length(hd(img))
    map = for {row, r} <- Enum.with_index(img), {char, c} <- Enum.with_index(row), into: %{}, do: {{r, c}, char}
    for r <- 0..(h-3), c <- 0..(w-20), Enum.all?(@monster, fn {dr, dc} -> map[{r+dr, c+dc}] == "#" end) do
      Enum.map(@monster, fn {dr, dc} -> {r+dr, c+dc} end)
    end |> List.flatten() |> Enum.uniq()
  end
end

Solution.main()
