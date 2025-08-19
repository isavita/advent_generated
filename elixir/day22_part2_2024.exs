
# frozen_string_literal: true
defmodule Main do
  use Bitwise

  @num_steps 2_000
  @pattern_count 130_321          # 19 ^ 4
  @mod 1 <<< 24
  @mod_mask @mod - 1
  @base 19
  @base2 @base * @base
  @base3 @base2 * @base

  # ----------------------------------------------------------------------
  # Pseudo‑random generator used in the original C program
  # ----------------------------------------------------------------------
  defp next_secret(s) do
    x = s * 64
    s = Bitwise.bxor(s, x) &&& @mod_mask
    x = div(s, 32)
    s = Bitwise.bxor(s, x) &&& @mod_mask
    x = s * 2_048
    Bitwise.bxor(s, x) &&& @mod_mask
  end

  # ----------------------------------------------------------------------
  # Encode four changes (each in -9..9) into a single index 0..130320
  # ----------------------------------------------------------------------
  defp encode(c1, c2, c3, c4) do
    (c1 + 9) +
      (c2 + 9) * @base +
      (c3 + 9) * @base2 +
      (c4 + 9) * @base3
  end

  # ----------------------------------------------------------------------
  # Build the whole price list (length @num_steps + 1) for one buyer
  # ----------------------------------------------------------------------
  defp build_prices(seed) do
    {_, rev_prices} =
      Enum.reduce(0..@num_steps, {seed, []}, fn _, {s, acc} ->
        price = rem(s, 10)
        {next_secret(s), [price | acc]}
      end)

    Enum.reverse(rev_prices)
  end

  # ----------------------------------------------------------------------
  # Main entry point
  # ----------------------------------------------------------------------
  def main do
    # ---- read all initial values ------------------------------------------------
    initials =
      case File.read("input.txt") do
        {:ok, content} ->
          content
          |> String.split(~r/\s+/, trim: true)
          |> Enum.map(&String.to_integer/1)

        {:error, _} ->
          []
      end

    if initials == [] do
      IO.puts("0")
      exit(:normal)
    end

    # ---- global sums stored in a fixed‑size Erlang array -------------------------
    global_sum = :array.new(@pattern_count, default: 0)

    # ---- process each buyer ------------------------------------------------------
    global_sum =
      Enum.reduce(initials, global_sum, fn seed, gsum ->
        prices = build_prices(seed)

        # changes[i] = prices[i+1] - prices[i]
        changes =
          Enum.zip(prices, tl(prices))
          |> Enum.map(fn {a, b} -> b - a end)

        # map of pattern index -> first price seen for this buyer
        local = %{}

        # walk through all windows of four consecutive changes
        max_i = @num_steps - 4

        local =
          Enum.reduce(0..max_i, local, fn i, acc ->
            c1 = Enum.at(changes, i)
            c2 = Enum.at(changes, i + 1)
            c3 = Enum.at(changes, i + 2)
            c4 = Enum.at(changes, i + 3)

            # all changes are already in [-9,9]; the check is kept for parity
            if abs(c1) <= 9 and abs(c2) <= 9 and abs(c3) <= 9 and abs(c4) <= 9 do
              idx = encode(c1, c2, c3, c4)

              case Map.fetch(acc, idx) do
                :error ->
                  price_at_end = Enum.at(prices, i + 4)
                  Map.put(acc, idx, price_at_end)

                {:ok, _} ->
                  acc
              end
            else
              acc
            end
          end)

        # merge the buyer's first‑occurrence prices into the global sums
        Enum.reduce(local, gsum, fn {idx, price}, arr ->
          current = :array.get(idx, arr)
          :array.set(idx, current + price, arr)
        end)
      end)

    # ---- find the maximum sum ----------------------------------------------------
    max_sum =
      0..(@pattern_count - 1)
      |> Enum.map(& :array.get(&1, global_sum))
      |> Enum.max()

    IO.puts(max_sum)
  end
end

Main.main()
