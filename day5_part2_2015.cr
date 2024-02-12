
file = File.read("input.txt").chomp

nice = 0

def passes_rule1(line)
  (0..line.size-3).any? do |i|
    to_match = line[i, 2]
    (i+2...line.size-1).any? do |j|
      line[j, 2] == to_match
    end
  end
end

file.each_line do |line|
  rule1 = passes_rule1(line)

  rule2 = (0..line.size-3).any? { |i| line[i] == line[i+2] }

  nice += 1 if rule1 && rule2
end

puts nice
