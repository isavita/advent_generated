def fft_phase(input)
  output = Array.new(input.length, 0)
  input.each_with_index do |_, i|
    sum = 0
    j = i
    while j < input.length
      sum += input[j, i + 1].sum
      j += 2 * (i + 1)
      break if j >= input.length
      sum -= input[j, i + 1].sum
      j += 2 * (i + 1)
    end
    output[i] = sum.abs % 10
  end
  output
end

def part1(input)
  signal = input.chars.map(&:to_i)
  100.times { signal = fft_phase(signal) }
  signal.take(8).join
end

def part2(input)
  offset = input[0, 7].to_i
  signal = (input * 10000)[offset..-1].chars.map(&:to_i)
  
  100.times do
    sum = signal.sum
    signal.each_with_index do |_, i|
      temp = sum
      sum -= signal[i]
      signal[i] = temp % 10
    end
  end
  
  signal.take(8).join
end

input = File.read('input.txt').strip
puts "Part 1: #{part1(input)}"
puts "Part 2: #{part2(input)}"
