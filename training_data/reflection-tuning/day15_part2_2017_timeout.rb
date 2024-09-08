class Generator
  DIVISOR = 2147483647

  def initialize(start, factor, multiple)
    @value = start
    @factor = factor
    @multiple = multiple
  end

  def next_value
    loop do
      @value = (@value * @factor) % DIVISOR
      return @value if @value % @multiple == 0
    end
  end
end

def count_matches(gen_a, gen_b, pairs)
  count = 0
  pairs.times do
    a = gen_a.next_value
    b = gen_b.next_value
    count += 1 if (a & 0xFFFF) == (b & 0xFFFF)
  end
  count
end

# Part 1
gen_a = Generator.new(516, 16807, 1)
gen_b = Generator.new(190, 48271, 1)
puts "Part 1: #{count_matches(gen_a, gen_b, 40_000_000)}"

# Part 2
gen_a = Generator.new(516, 16807, 4)
gen_b = Generator.new(190, 48271, 8)
puts "Part 2: #{count_matches(gen_a, gen_b, 5_000_000)}"
