
def rotate(input : String) : String
  parts = input.split("/")
  size = parts.size
  new_parts = Array(String).new(size, "")
  size.times do |x|
    new_row = ""
    (size - 1).downto(0) do |y|
      new_row += parts[y][x]
    end
    new_parts[x] = new_row
  end
  new_parts.join("/")
end

def flip(input : String) : String
  input.split("/").map(&.reverse).join("/")
end

def enhance(input : String, rules : Hash(String, String)) : String
  8.times do |i|
    return rules[input] if rules.has_key?(input)
    input = rotate(input)
    input = flip(input) if i == 3
  end
  ""
end

def main
  rules = Hash(String, String).new
  File.open("./input.txt", "r") do |file|
    file.each_line do |line|
      parts = line.strip.split(" => ")
      rules[parts[0]] = parts[1]
    end
  end

  grid = [
    ".#.",
    "..#",
    "###",
  ]

  5.times do
    sub_size, new_size = if grid.size.even?
                           {2, grid.size // 2 * 3}
                         else
                           {3, grid.size // 3 * 4}
                         end

    new_grid = Array(String).new(new_size, "")

    (0...grid.size).step(sub_size) do |y|
      (0...grid.size).step(sub_size) do |x|
        square = Array(String).new(sub_size) { |dy| grid[y + dy][x, sub_size] }
        new_square = enhance(square.join("/"), rules)
        new_square.split("/").each_with_index do |row, dy|
          new_grid[y // sub_size * (sub_size + 1) + dy] += row
        end
      end
    end
    grid = new_grid
  end
  puts grid.join.count('#')
end

main
