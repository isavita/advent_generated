
file = File.read("input.txt").chomp

nice = 0
disallow_pattern = Regex.new("(ab|cd|pq|xy)")
file.lines.each do |line|
  vowels = line.count("aeiou")
  has_double = false
  (0...line.size-1).each do |i|
    has_double = true if line[i] == line[i+1]
  end

  nice += 1 if vowels >= 3 && !disallow_pattern.match(line) && has_double
end

puts nice
