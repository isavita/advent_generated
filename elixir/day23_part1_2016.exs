defmodule SafeCracking do
  def call do
    "input.txt"
    |> File.read!()
    |> calculate()
    |> Keyword.get(:a)
  end

  defp parse_line(line) do
    line
    |> String.split(" ")
    |> Enum.map(&parse_elem/1)
  end

  defp parse_elem(value), do: parse_elem(value, Integer.parse(value))
  defp parse_elem(value, :error), do: String.to_atom(value)
  defp parse_elem(_value, {int, ""}), do: int

  defp loop(list), do: loop(list, 0, init())
  defp loop(list, pointer, info) when pointer >= length(list), do: info
  defp loop(list, pointer, info) do
    item = Enum.at(list, pointer)
    {list, pointer, info} = execute(item, list, pointer, info)
    loop(list, pointer, info)
  end

  defp execute([:cpy, _value, elem], list, pointer, info) when is_integer(elem) do
    IO.puts("This shouldn't happen")
    1 / 0
    {list, pointer + 1, info}
  end
  defp execute([:cpy, value, elem], list, pointer, info) do
    value = parse_value(info, value)
    info = set(info, elem, value)
    {list, pointer + 1, info}
  end
  defp execute([:inc, elem], list, pointer, info) do
    info = set(info, elem, get(info, elem) + 1)
    {list, pointer + 1, info}
  end
  defp execute([:dec, elem], list, pointer, info) do
    info = set(info, elem, get(info, elem) - 1)
    {list, pointer + 1, info}
  end
  defp execute([:jnz, value, change], list, pointer, info) do
    value = parse_value(info, value)
    change = parse_value(info, change)
    pointer = if (value == 0), do: pointer + 1, else: pointer + change
    {list, pointer, info}
  end
  defp execute([:tgl, elem], list, pointer, info) do
    toggle_index = get(info, elem) + pointer
    item = Enum.at(list, toggle_index)
    list = toggle_elem(list, toggle_index, item)
    {list, pointer + 1, info}
  end

  defp toggle_elem(list, _, nil), do: list
  defp toggle_elem(list, index, item) do
    item = toggle_instruction(item)
    List.replace_at(list, index, item)
  end

  defp toggle_instruction([:inc, elem]), do: [:dec, elem]
  defp toggle_instruction([_, elem]), do: [:inc, elem]
  defp toggle_instruction([:jnz, a, b]), do: [:cpy, a, b]
  defp toggle_instruction([_, a, b]), do: [:jnz, a, b]

  defp get(info, elem), do: Keyword.get(info, elem)
  defp set(info, elem, value) do
    Keyword.put(info, elem, value)
  end

  defp parse_value(_info, value) when is_integer(value), do: value
  defp parse_value(info, value), do: get(info, value)

  defp init do
    [a: 7, b: 0, c: 0, d: 0]
  end

  defp calculate(input) do
    input
      |> String.split("\n")
      |> List.delete_at(-1)
      |> Enum.map(&parse_line/1)
      |> loop
  end
end
