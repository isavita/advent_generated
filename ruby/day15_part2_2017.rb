
input = File.readlines('input.txt').map(&:to_i)

def generator(prev, factor, criteria)
  loop do
    prev = (prev * factor) % 2147483647
    yield prev if prev % criteria == 0
  end
end

gen_a = Enumerator.new { |y| generator(input[0], 16807, 4) { |val| y << val } }
gen_b = Enumerator.new { |y| generator(input[1], 48271, 8) { |val| y << val } }

puts gen_a.take(5_000_000).zip(gen_b.take(5_000_000)).count { |a, b| a & 0xFFFF == b & 0xFFFF }
