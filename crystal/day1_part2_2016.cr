file = File.open("input.txt")
instructions = file.gets_to_end.chomp.split(", ")

def first_revisited_distance(instructions)
  pos = {0, 0}
  visited = {pos => true}
  directions = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]
  dir_index = 0

  instructions.each do |instruction|
    turn = instruction[0]
    blocks = instruction[1..-1].to_i

    if turn == 'R'
      dir_index = (dir_index + 1) % 4
    else
      dir_index = (dir_index - 1 + 4) % 4
    end

    blocks.times do
      pos = {pos[0] + directions[dir_index][0], pos[1] + directions[dir_index][1]}
      if visited.has_key?(pos)
        return pos[0].abs + pos[1].abs
      end
      visited[pos] = true
    end
  end

  -1 # No location visited twice
end

puts first_revisited_distance(instructions)