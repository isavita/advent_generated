
def next_row(row)
  ('.' + row + '.').chars.each_cons(3).map { |l, c, r|
    l == r ? '.' : '^'
  }.join
end

def count_safe_tiles(input, rows)
  (1...rows).reduce([input]) { |acc, _|
    acc << next_row(acc.last)
  }.join.count('.')
end

input = File.read('input.txt').strip
puts count_safe_tiles(input, 40)
