
def next_code(code)
  (code * 252533) % 33554393
end

def find_position(row, column)
  diagonal = row + column - 1
  total_steps = (diagonal * (diagonal - 1)) / 2
  total_steps + column
end

def find_code(row, column)
  position = find_position(row, column)
  code = 20151125
  (position - 1).times { code = next_code(code) }
  code
end

input = File.read("input.txt").scan(/\d+/).map(&:to_i)
row, column = input
puts find_code(row, column)
