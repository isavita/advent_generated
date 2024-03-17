frequency = 0

File.read_lines("input.txt").each do |line|
  frequency += line.to_i
end

puts frequency