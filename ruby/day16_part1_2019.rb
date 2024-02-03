
input = File.read('input.txt').chomp.split('').map(&:to_i)

def apply_phase(input)
  base_pattern = [0, 1, 0, -1]
  output = []

  input.each_with_index do |_, i|
    pattern = base_pattern.flat_map { |n| [n] * (i + 1) }
    pattern.rotate!
    sum = input.zip(pattern.cycle).map { |a, b| a * b }.sum
    output << sum.abs % 10
  end

  output
end

100.times do
  input = apply_phase(input)
end

puts input.take(8).join
