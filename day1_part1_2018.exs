
defmodule ChronalCalibration do
  def call do
    "input.txt"
    |> File.read!()
    |> String.split()
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end
end
