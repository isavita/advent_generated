
def fft(input)
  base_pattern = [0, 1, 0, -1]
  output = []
  input.length.times do |i|
    pattern = base_pattern.flat_map { |x| [x] * (i + 1) }
    sum = 0
    input.length.times do |j|
      sum += input[j] * pattern[(j + 1) % pattern.length]
    end
    output << sum.abs % 10
  end
  output
end

def part_one(input)
  100.times { input = fft(input) }
  input[0, 8].join
end

def part_two(input)
  offset = input[0, 7].join.to_i
  input = (input * 10000)[offset..-1]
  100.times do
    sum = 0
    (input.length - 1).downto(0) do |i|
      sum += input[i]
      input[i] = sum % 10
    end
  end
  input[0, 8].join
end

input = File.read("input.txt").chomp.chars.map(&:to_i)
puts part_one(input.dup)
puts part_two(input.dup)
