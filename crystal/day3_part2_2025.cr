
require "big"

TARGET = 12
total = BigInt.new(0)

File.each_line("input.txt") do |line|
  # Trim end‑of‑line characters and any trailing non‑digits
  line = line.rstrip
  while line.size > 0 && (line[-1] < '0' || line[-1] > '9')
    line = line[0...-1]
  end

  next if line.size < TARGET

  rem = line.size - TARGET
  stack = [] of Char

  line.each_char do |ch|
    while rem > 0 && !stack.empty? && stack[-1] < ch
      stack.pop
      rem -= 1
    end
    stack << ch
  end

  # Keep only the first TARGET digits
  num_str = stack[0, TARGET].join
  total += BigInt.new(num_str)
end

puts total
