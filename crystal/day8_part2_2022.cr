require "file"

grid = {} of Tuple(Int32, Int32) => Int32
y = 0
File.open("input.txt", "r") do |file|
  file.each_line do |line|
    x = 0
    line.each_char do |c|
      grid[{x, y}] = c.to_i
      x += 1
    end
    y += 1
  end
end

max_score = 0
Neighbors4 = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
grid.each do |p, _|
  score = 1
  Neighbors4.each do |n|
    next_p, view = p, 0
    loop do
      next_p = {next_p[0] + n[0], next_p[1] + n[1]}
      if grid.has_key?(next_p)
        view += 1
        if grid[next_p] >= grid[p]
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