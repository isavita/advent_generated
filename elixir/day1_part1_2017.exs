
defmodule CaptchaSolver do
  def call do
    "input.txt"
    |> File.read!()
    |> String.trim()
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> solve_captcha()
  end

  defp solve_captcha(list) do
    list
    |> Enum.zip(Enum.concat(Enum.drop(list, 1), [hd(list)]))
    |> Enum.filter(fn {a, b} -> a == b end)
    |> Enum.map(fn {a, _} -> a end)
    |> Enum.sum()
  end
end
