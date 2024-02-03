
def next_row(previous)
  ('.' + previous + '.').chars.each_cons(3).map { |l, c, r| l == r ? '.' : '^' }.join
end

def count_safe_tiles(input, rows)
  current_row = input.strip
  total_safe = current_row.count('.')
  (rows - 1).times do
    current_row = next_row(current_row)
    total_safe += current_row.count('.')
  end
  total_safe
end

input = File.read('input.txt')
puts count_safe_tiles(input, 40)
puts count_safe_tiles(input, 400000)
