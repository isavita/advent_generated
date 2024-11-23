
defmodule CathodeRayTube do
  def run do
    {:ok, content} = File.read("input.txt")
    instructions = parse_instructions(content)

    {signal_strength, crt_output} = simulate(instructions)

    IO.puts("Sum of signal strengths: #{signal_strength}")
    IO.puts("CRT Output:")
    IO.puts(crt_output)
  end

  defp parse_instructions(content) do
    content
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_instruction/1)
  end

  defp parse_instruction("noop"), do: {:noop, 0}
  defp parse_instruction("addx " <> value), do: {:addx, String.to_integer(value)}

  defp simulate(instructions) do
    {signal_strength, crt_output} =
      Enum.reduce(instructions, {1, 1, 0, ""}, fn instruction, {cycle, x, signal_strength, crt_output} ->
        {new_cycle, new_x, new_signal_strength, new_crt_output} =
          case instruction do
            {:noop, _} ->
              {cycle + 1, x, signal_strength, draw_pixel(cycle, x, crt_output)}

            {:addx, value} ->
              cycle1_output = draw_pixel(cycle, x, crt_output)
              cycle2_output = draw_pixel(cycle + 1, x, cycle1_output)
              {cycle + 2, x + value, signal_strength, cycle2_output}
          end

        new_signal_strength =
          if rem(new_cycle, 40) == 20 do
            new_signal_strength + new_cycle * new_x
          else
            new_signal_strength
          end

        {new_cycle, new_x, new_signal_strength, new_crt_output}
      end)

    {signal_strength, crt_output}
  end

  defp draw_pixel(cycle, x, crt_output) do
    pixel_position = rem(cycle - 1, 40)
    sprite_positions = (x - 1)..(x + 1)

    pixel =
      if pixel_position in sprite_positions do
        "#"
      else
        "."
      end

    crt_output <> pixel <> if(rem(cycle, 40) == 0, do: "\n", else: "")
  end
end

CathodeRayTube.run()
