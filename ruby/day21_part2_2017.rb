
def enhance(input, rules)
  @memo ||= {}
  return @memo[input] if @memo[input]

  original = input
  4.times do
    if rules[input]
      @memo[original] = rules[input]
      return rules[input]
    end
    input = rotate(input)
  end
  input = flip(input)
  4.times do
    if rules[input]
      @memo[original] = rules[input]
      return rules[input]
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
    (size - 1).downto(0) do |y|
      new_parts[x] += parts[y][x]
    end
  end
  new_parts.join("/")
end

def flip(input)
  parts = input.split("/")
  parts.map! { |part| part.reverse }
  parts.join("/")
end

rules = {}
File.open("input.txt").each do |line|
  parts = line.strip.split(" => ")
  rules[parts[0]] = parts[1]
end

grid = [".#.", "..#", "###"]

18.times do
  if grid.size % 2 == 0
    sub_size = 2
    new_size = grid.size / 2 * 3
  else
    sub_size = 3
    new_size = grid.size / 3 * 4
  end

  new_grid = Array.new(new_size) { "" }

  (0...grid.size).step(sub_size) do |y|
    (0...grid.size).step(sub_size) do |x|
      square = (0...sub_size).map { |dy| grid[y + dy][x, sub_size] }
      new_square = enhance(square.join("/"), rules)
      new_square.split("/").each_with_index do |row, dy|
        new_grid[y / sub_size * (sub_size + 1) + dy] += row
      end
    end
  end
  grid = new_grid
end

count = grid.sum { |row| row.count("#") }
puts count
