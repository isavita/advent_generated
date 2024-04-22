file = File.open("input.txt")
screen_width = 50
screen_height = 6
screen = Array.new(screen_height) { Array.new(screen_width, false) }

file.each_line do |line|
  process_instruction(line, screen, screen_width, screen_height)
end

puts count_lit_pixels(screen)

def process_instruction(instruction, screen, screen_width, screen_height)
  if instruction =~ /rect (\d+)x(\d+)/
    a = $1.to_i
    b = $2.to_i
    rect(screen, a, b)
  elsif instruction =~ /rotate row y=(\d+) by (\d+)/
    a = $1.to_i
    b = $2.to_i
    rotate_row(screen, a, b, screen_width)
  elsif instruction =~ /rotate column x=(\d+) by (\d+)/
    a = $1.to_i
    b = $2.to_i
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
  screen[row].each_with_index do |val, i|
    temp[(i + shift) % screen_width] = val
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

def count_lit_pixels(screen)
  count = 0
  screen.each do |row|
    row.each do |pixel|
      count += 1 if pixel
    end
  end
  count
end