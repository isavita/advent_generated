
defmodule Day16 do
  def read_input(filename) do
    {:ok, contents} = File.read(filename)
    contents
  end

  def parse_rule(line) do
    [name | ranges] = Regex.run(~r/^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/, line, capture: :all_but_first)
    [r1min, r1max, r2min, r2max] = Enum.map(ranges, &String.to_integer/1)
    {name, [{r1min, r1max}, {r2min, r2max}]}
  end

  def parse_ticket(line) do
    line |> String.split(",") |> Enum.map(&String.to_integer/1)
  end

  def valid_for_range?(value, {min, max}), do: value >= min and value <= max

  def valid_for_rule?(value, ranges) do
    Enum.any?(ranges, &valid_for_range?(value, &1))
  end

  def valid_for_any_rule?(value, rules) do
    Enum.any?(rules, fn {_, ranges} -> valid_for_rule?(value, ranges) end)
  end

  def valid_ticket?(ticket, rules) do
    Enum.all?(ticket, &valid_for_any_rule?(&1, rules))
  end

  def solve_field_positions(rules, tickets) do
    num_fields = length(hd(tickets))

    possible_positions =
      for {name, ranges} <- rules, i <- 0..(num_fields - 1) do
        if Enum.all?(tickets, &(valid_for_rule?(Enum.at(&1, i), ranges))) do
          {name, i}
        end
      end
      |> Enum.filter(& &1)
      |> Enum.group_by(fn {name, _} -> name end, fn {_, i} -> i end)

    solve_positions_recursive(possible_positions, %{})
  end

  def solve_positions_recursive(possible_positions, solved) do
    if map_size(possible_positions) == 0 do
      solved
    else
      {name, [pos]} = Enum.find(possible_positions, fn {_, poss} -> length(poss) == 1 end)

      new_solved = Map.put(solved, name, pos)

      new_possible =
        possible_positions
        |> Map.delete(name)
        |> Enum.map(fn {n, p} -> {n, List.delete(p, pos)} end)
        |> Map.new()

      solve_positions_recursive(new_possible, new_solved)
    end
  end

  def calculate_departure_product(ticket, field_positions) do
    field_positions
    |> Enum.filter(fn {name, _} -> String.starts_with?(name, "departure") end)
    |> Enum.map(fn {_, pos} -> Enum.at(ticket, pos) end)
    |> Enum.reduce(1, &*/2)
  end

  def main(filename) do
    input = read_input(filename)
    [rules_str, my_ticket_str, nearby_tickets_str] = String.split(input, "\n\n", trim: true)

    rules =
      rules_str
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_rule/1)

    my_ticket =
      my_ticket_str
      |> String.split("\n", trim: true)
      |> List.last()
      |> parse_ticket()

    nearby_tickets =
      nearby_tickets_str
      |> String.split("\n", trim: true)
      |> Enum.drop(1)
      |> Enum.map(&parse_ticket/1)
      |> Enum.filter(&valid_ticket?(&1, rules))

    field_positions = solve_field_positions(rules, nearby_tickets)
    calculate_departure_product(my_ticket, field_positions)
  end
end

IO.puts(Day16.main("input.txt"))
