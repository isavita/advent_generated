file = File.open("input.txt")
screen_width = 50
screen_height = 6
screen = Array.new(screen_height) { Array.new(screen_width, false) }

def display_screen(screen)
  screen.each do |row|
    row.each do |pixel|
      print pixel ? "#" : "."
    end
    puts
  end
end

def process_instruction(instruction, screen, screen_width, screen_height)
  if instruction.starts_with?("rect ")
    a, b = instruction[5..-1].split("x").map(&.to_i)
    rect(screen, a, b)
  elsif instruction.starts_with?("rotate row y=")
    a, b = instruction[13..-1].split(" by ").map(&.to_i)
    rotate_row(screen, a, b, screen_width)
  elsif instruction.starts_with?("rotate column x=")
    a, b = instruction[16..-1].split(" by ").map(&.to_i)
    rotate_column(screen, a, b, screen_height)
  end
end

def rect(screen, a, b)
  b.times do |y|
    a.times do |x|
      screen[y][x] = true
    end
  end
end

def rotate_row(screen, row, shift, screen_width)
  temp = Array.new(screen_width, false)
  screen[row].each_with_index do |pixel, i|
    temp[(i + shift) % screen_width] = pixel
  end
  screen[row] = temp
end

def rotate_column(screen, col, shift, screen_height)
  temp = Array.new(screen_height, false)
  screen.each_with_index do |row, i|
    temp[(i + shift) % screen_height] = row[col]
  end
  screen.each_with_index do |row, i|
    row[col] = temp[i]
  end
end

file.each_line do |line|
  process_instruction(line, screen, screen_width, screen_height)
end

display_screen(screen)