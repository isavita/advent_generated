require "file_utils"

MEMO = {} of String => String
RULES = {} of String => String

File.open("input.txt") do |file|
  file.each_line do |line|
    parts = line.chomp.split(" => ")
    RULES[parts[0]] = parts[1]
  end
end

grid = [
  ".#.",
  "..#",
  "###",
]

18.times do
  if grid.size.even?
    sub_size = 2
    new_size = grid.size // 2 * 3
  else
    sub_size = 3
    new_size = grid.size // 3 * 4
  end

  new_grid = Array.new(new_size) { "" }

  (0...grid.size).step(sub_size).each do |y|
    (0...grid.size).step(sub_size).each do |x|
      square = (0...sub_size).map { |dy| grid[y + dy][x, sub_size] }
      new_square = enhance(square.join("/"), RULES)
      new_square.split("/").each_with_index do |row, dy|
        new_grid[y // sub_size * (sub_size + 1) + dy] += row
      end
    end
  end

  grid = new_grid
end

count = 0
grid.each do |row|
  row.each_char do |pixel|
    count += 1 if pixel == '#'
  end
end

puts count

def enhance(input, rules)
  return MEMO[input] if MEMO[input]?

  original = input
  4.times do
    if output = rules[input]?
      MEMO[original] = output
      return output
    end
    input = rotate(input)
  end

  input = flip(input)
  4.times do
    if output = rules[input]?
      MEMO[original] = output
      return output
    end
    input = rotate(input)
  end

  ""
end

def rotate(input)
  parts = input.split("/")
  size = parts.size
  new_parts = Array.new(size) { "" }
  size.times do |x|
    new_row = ""
    (size - 1).downto(0) do |y|
      new_row += parts[y][x]
    end
    new_parts[x] = new_row
  end
  new_parts.join("/")
end

def flip(input)
  parts = input.split("/")
  parts.map! { |part| part.reverse }
  parts.join("/")
end