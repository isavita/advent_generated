
input = File.read('input.txt').split("\n\n")

# Part One
puts input.map { |group| group.split("\n").join.chars.uniq.count }.sum

# Part Two
puts input.map { |group| group.split.map(&:chars).inject(:&).count }.sum
