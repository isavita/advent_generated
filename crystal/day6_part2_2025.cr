
lines = File.read_lines("input.txt")
exit if lines.empty?

num_rows = lines.size
max_cols = lines.map(&.size).max

grid = Array.new(num_rows) { Array(Char).new(max_cols, ' ') }
lines.each_with_index do |line, i|
  line.each_char.with_index { |ch, j| grid[i][j] = ch }
end

grand_total = 0_i64
col = 0
while col < max_cols
  empty = true
  i = 0
  while i < num_rows
    if grid[i][col] != ' '
      empty = false
      break
    end
    i += 1
  end
  if empty
    col += 1
    next
  end

  start = col
  while col < max_cols
    empty = true
    i = 0
    while i < num_rows
      if grid[i][col] != ' '
        empty = false
        break
      end
      i += 1
    end
    break if empty
    col += 1
  end
  finish = col

  operator = '+'
  (start...finish).each do |j|
    c = grid[num_rows - 1][j]
    if c == '+' || c == '*'
      operator = c
      break
    end
  end

  numbers = [] of Int64
  (finish - 1).downto(start) do |j|
    sb = String.build do |s|
      (0...num_rows - 1).each { |i| s << grid[i][j] }
    end
    str = sb.strip
    numbers << str.to_i64 unless str.empty?
  end

  unless numbers.empty?
    result = numbers[0]
    (1...numbers.size).each do |k|
      result = operator == '+' ? result + numbers[k] : result * numbers[k]
    end
    grand_total += result
  end
end

puts grand_total
