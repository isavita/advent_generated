
defmodule SantaFloor do
  def call do
    "input.txt"
    |> File.read!()
    |> String.graphemes()
    |> Enum.reduce(0, fn
      "(", acc -> acc + 1
      ")", acc -> acc - 1
    end)
  end
end
