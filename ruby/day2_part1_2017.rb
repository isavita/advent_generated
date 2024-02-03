
spreadsheet = File.readlines('input.txt').map { |line| line.split.map(&:to_i) }
puts spreadsheet.map { |row| row.max - row.min }.sum
