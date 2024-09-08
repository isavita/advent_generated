def parse_input(input)
  map, instructions = input.split("\n\n")
  map = map.split("\n").map(&:chars)
  instructions = instructions.scan(/\d+|\D/)
  [map, instructions]
end

def find_start(map)
  map[0].index { |c| c == '.' }
end

def move(map, row, col, facing, steps)
  directions = [[0, 1], [1, 0], [0, -1], [-1, 0]]
  height, width = map.size, map[0].size

  steps.times do
    new_row = (row + directions[facing][0]) % height
    new_col = (col + directions[facing][1]) % width

    # Wrap around if necessary
    while map[new_row][new_col] == ' ' || map[new_row][new_col].nil?
      new_row = (new_row + directions[facing][0]) % height
      new_col = (new_col + directions[facing][1]) % width
    end

    break if map[new_row][new_col] == '#'
    row, col = new_row, new_col
  end

  [row, col]
end

def solve(input)
  map, instructions = parse_input(input)
  row, col = 0, find_start(map)
  facing = 0

  instructions.each do |instruction|
    if instruction =~ /\d+/
      row, col = move(map, row, col, facing, instruction.to_i)
    else
      facing = (facing + (instruction == 'R' ? 1 : -1)) % 4
    end
  end

  1000 * (row + 1) + 4 * (col + 1) + facing
end

# Example usage:
# input = File.read('input.txt')
# puts solve(input)
