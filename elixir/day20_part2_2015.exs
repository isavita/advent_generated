defmodule PresentDelivery do
  def find_house(target) do
    target = div(target, 11)
    houses = :array.new(target + 1, default: 0)

    1..target
    |> Enum.reduce(houses, fn elf, houses ->
      1..min(50, div(target, elf))
      |> Enum.reduce(houses, fn multiplier, houses ->
        house = elf * multiplier
        :array.set(house, :array.get(house, houses) + elf, houses)
      end)
    end)
    |> :array.to_list()
    |> Enum.find_index(&(&1 >= target))
    |> IO.puts()
  end

  def main do
    {:ok, input} = File.read("input.txt")
    target = String.trim(input) |> String.to_integer()
    find_house(target)
  end
end

PresentDelivery.main()