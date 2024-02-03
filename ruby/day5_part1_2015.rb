nice_strings = 0

File.open("input.txt").each do |line|
  nice_strings += 1 if line.scan(/[aeiou]/).count >= 3 && line =~ /(.)\1/ && line !~ /(ab|cd|pq|xy)/
end

puts nice_strings