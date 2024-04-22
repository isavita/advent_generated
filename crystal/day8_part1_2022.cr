require "file_utils"

grid = {} of Tuple(Int32, Int32) => Int32
visible = {} of Tuple(Int32, Int32) => Nil

File.open("input.txt", "r") do |file|
  y = 0
  file.each_line do |line|
    x = 0
    line.each_char do |char|
      grid[{x, y}] = char.to_i
      x += 1
    end
    y += 1
  end
end

neighbors = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]

grid.each_key do |p|
  neighbors.each do |n|
    next_p = p
    loop do
      next_p = {next_p[0] + n[0], next_p[1] + n[1]}
      if grid.has_key?(next_p)
        if grid[next_p] >= grid[p]
          break
        end
      else
        visible[p] = nil
        break
      end
    end
  end
end

puts visible.size