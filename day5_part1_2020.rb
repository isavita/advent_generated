
seats = File.readlines('input.txt').map { |line| line.chomp.tr('FBLR', '0101').to_i(2) }
puts seats.max
