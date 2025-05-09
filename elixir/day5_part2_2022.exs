
defmodule Solution do
  def main do
    File.read!("input.txt")
    |> String.split("\n\n")
    |> parse_and_solve()
    |> IO.puts()
  end

  def parse_and_solve([stack_data, step_data]) do
    stacks = parse_stacks(stack_data)
    steps = parse_steps(step_data)
    final_stacks = Enum.reduce(steps, stacks, &apply_step/2)
    get_top_crates(final_stacks)
  end

  defp parse_stacks(data) do
    lines = String.split(data, "\n")
    num_stacks = (String.length(List.first(lines)) + 1) |> div(4)
    initial_stacks = List.duplicate([], num_stacks)

    lines
    |> Enum.reverse()
    |> Enum.reduce(initial_stacks, fn line, acc_stacks ->
      line
      |> String.to_charlist()
      |> Enum.with_index()
      |> Enum.reduce(acc_stacks, fn {char, index}, inner_acc_stacks ->
        if char >= ?A and char <= ?Z and rem(index - 1, 4) == 0 do
          stack_idx = div(index - 1, 4)
          List.update_at(inner_acc_stacks, stack_idx, fn stack -> [char | stack] end)
        else
          inner_acc_stacks
        end
      end)
    end)
  end

  defp parse_steps(data) do
    data
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [_, n, _, frm, _, to] = String.split(line)
      %{
        n: String.to_integer(n),
        frm: String.to_integer(frm) - 1,
        to: String.to_integer(to) - 1
      }
    end)
  end

  defp apply_step(%{n: n, frm: frm, to: to}, current_stacks) do
    source_stack = Enum.at(current_stacks, frm)
    target_stack = Enum.at(current_stacks, to)

    {moved_crates, remaining_source_stack} = Enum.split(source_stack, n)
    new_target_stack = moved_crates ++ target_stack

    current_stacks
    |> List.replace_at(frm, remaining_source_stack)
    |> List.replace_at(to, new_target_stack)
  end

  defp get_top_crates(stacks) do
    stacks
    |> Enum.map(&List.first/1)
    |> List.to_string()
  end
end

Solution.main()
