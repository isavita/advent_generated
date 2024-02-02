
defmodule Day2 do
  def call do
    File.read!("input.txt")
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_dimensions/1)
    |> Enum.reduce(0, &calculate_paper_needed/2)
  end

  defp parse_dimensions(line) do
    [l, w, h] = String.split(line, "x") |> Enum.map(&String.to_integer/1)
    {l, w, h}
  end

  defp calculate_paper_needed({l, w, h}, acc) do
    area = 2*l*w + 2*w*h + 2*h*l
    slack = Enum.min([l*w, w*h, h*l])
    acc + area + slack
  end
end
