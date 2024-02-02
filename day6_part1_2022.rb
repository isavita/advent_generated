
data = File.read("input.txt").chomp
index = data.chars.each_with_index.find { |char, i| i >= 3 && data[i-3..i].chars.uniq.length == 4 }[1]
puts index + 1
