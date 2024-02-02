
puts File.read("input.txt").split("\n\n").sum { |group| group.split("\n").join.chars.uniq.count }
