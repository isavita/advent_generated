require "file_utils"

def repeat_input(input, times)
  (input.chars.map(&.to_i) * times).to_a
end

input = File.read("input.txt").chomp
repeated_input = repeat_input(input, 10_000)
offset = input[0, 7].to_i

100.times do
  sum = 0
  (repeated_input.size - 1).downto(offset) do |i|
    sum += repeated_input[i]
    repeated_input[i] = sum % 10
  end
end

puts repeated_input[offset, 8].join