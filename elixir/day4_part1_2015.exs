
defmodule StockingStuffer do
  def call do
    input = File.read!("input.txt")
    secret_key = String.trim(input)

    Enum.find(1..10000000, fn i ->
      hash = :crypto.hash(:md5, secret_key <> Integer.to_string(i))
      hash_hex = Base.encode16(hash, case: :lower)
      String.starts_with?(hash_hex, "00000")
    end)
  end
end
