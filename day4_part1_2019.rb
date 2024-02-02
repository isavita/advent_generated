
input = File.read("input.txt").split("-").map(&:to_i)
count = 0

(input[0]..input[1]).each do |num|
  digits = num.to_s.split("").map(&:to_i)
  next if digits != digits.sort
  next unless digits.chunk_while { |a, b| a == b }.any? { |chunk| chunk.size >= 2 }
  count += 1
end

puts count
