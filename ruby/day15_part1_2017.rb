
input = File.readlines('input.txt').map(&:to_i)

def generate_next(prev, factor)
  (prev * factor) % 2147483647
end

count = 0
a = input[0]
b = input[1]

40_000_000.times do
  a = generate_next(a, 16807)
  b = generate_next(b, 48271)
  count += 1 if a & 0xFFFF == b & 0xFFFF
end

puts count
