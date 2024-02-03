
require 'matrix'

NEIGHBORS4 = [Vector[0, 1], Vector[0, -1], Vector[1, 0], Vector[-1, 0]]

grid = {}
y = 0

File.open("input.txt", "r") do |file|
  file.each_line do |line|
    line.chomp.each_char.with_index do |b, x|
      grid[Vector[x, y]] = b.to_i
    end
    y += 1
  end
end

max_score = 0

grid.keys.each do |p|
  score = 1
  NEIGHBORS4.each do |n|
    view = 0
    next_pos = p
    loop do
      next_pos += n
      if grid.key?(next_pos)
        view += 1
        if grid[next_pos] >= grid[p]
          score *= view
          break
        end
      else
        score *= view
        break
      end
    end
  end

  max_score = score if score > max_score
end

puts max_score
