require "file_utils"

def apply_fft(input)
  base_pattern = [0, 1, 0, -1]
  output = [] of Int32
  input.each_index do |i|
    sum = 0
    input.each_with_index do |val, j|
      pattern_value = base_pattern[((j + 1) / (i + 1)).to_i % base_pattern.size]
      sum += val * pattern_value
    end
    output << (sum.abs % 10)
  end
  output
end

input = File.read("input.txt").chomp.chars.map(&.to_i32)

100.times { input = apply_fft(input) }

puts input[0, 8].join