
data = File.read("input.txt").strip

matches = data.match(%r{row (\d+), column (\d+)})
raise "Invalid input format." if matches.nil?

row = matches[1].to_i
column = matches[2].to_i

def get_position(row, column)
  (row + column - 2) * (row + column - 1) / 2 + column
end

def get_code(position)
  start_code = 20151125
  multiplier = 252533
  modulus = 33554393

  code = start_code.to_i64
  (1...position).each do
    code = (code * multiplier) % modulus
  end

  code
end

pos = get_position(row, column)
code = get_code(pos)

puts code
