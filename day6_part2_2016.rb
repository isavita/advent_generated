
input = File.readlines('input.txt').map(&:chomp)

puts input.map(&:chars).transpose.map { |column| column.group_by(&:itself).transform_values(&:size).min_by(&:last).first }.join
