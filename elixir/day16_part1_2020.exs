defmodule Day16 do
  def read_input do
    [rules, _your_ticket, nearby_tickets] =
      File.read!("input.txt")
      |> String.trim()
      |> String.split("\n\n")

    {parse_rules(rules), parse_tickets(nearby_tickets)}
  end

  defp parse_rules(rules) do
    Enum.map(String.split(rules, "\n"), fn rule ->
      [_, ranges] = String.split(rule, ": ")
      Enum.map(String.split(ranges, " or "), fn range ->
        [min, max] = String.split(range, "-")
        {String.to_integer(min), String.to_integer(max)}
      end)
    end)
    |> List.flatten()
  end

  defp parse_tickets(tickets) do
    tickets
    |> String.split("\n", trim: true)
    |> Enum.drop(1)
    |> Enum.map(fn ticket ->
      ticket
      |> String.split(",")
      |> Enum.map(fn value -> String.to_integer(value) end)
    end)
  end

  defp invalid_values(tickets, rules) do
    Enum.flat_map(tickets, fn ticket ->
      Enum.filter(ticket, fn value ->
        Enum.all?(rules, fn {min, max} ->
          value < min or value > max
        end)
      end)
    end)
  end

  def call do
    {rules, nearby_tickets} = read_input()
    error_rate = invalid_values(nearby_tickets, rules) |> Enum.sum()
    IO.puts("Ticket scanning error rate: #{error_rate}")
  end
end

Day16.call()
