defmodule TransparentOrigami do
  def call do
    {dots, instructions} = File.read!("input.txt")
                           |> parse_input()
    dots_after_fold = fold_paper(dots, hd(instructions))
    IO.puts("Visible dots after first fold: #{Enum.count(dots_after_fold)}")
  end

  defp parse_input(input) do
    [dot_section, instruction_section] = String.split(input, "\n\n", parts: 2, trim: true)

    dots = dot_section
           |> String.split("\n", trim: true)
           |> Enum.reject(&(&1 == ""))
           |> Enum.map(&String.split(&1, ",", trim: true))
           |> Enum.map(fn [x, y] -> {String.to_integer(x), String.to_integer(y)} end)

    instructions = instruction_section
                   |> String.split("\n", trim: true)
                   |> Enum.reject(&(&1 == ""))
                   |> Enum.map(fn
                     "fold along x=" <> x -> {:x, String.to_integer(x)}
                     "fold along y=" <> y -> {:y, String.to_integer(y)}
                   end)

    {dots, instructions}
  end

  defp fold_paper(dots, {:x, fold_line}) do
    Enum.map(dots, fn {x, y} ->
      if x > fold_line, do: {2 * fold_line - x, y}, else: {x, y}
    end)
    |> Enum.uniq()
  end

  defp fold_paper(dots, {:y, fold_line}) do
    Enum.map(dots, fn {x, y} ->
      if y > fold_line, do: {x, 2 * fold_line - y}, else: {x, y}
    end)
    |> Enum.uniq()
  end
end

TransparentOrigami.call()
