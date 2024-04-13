SCREEN_WIDTH = 50
SCREEN_HEIGHT = 6

def display_screen(screen)
  screen.each do |row|
    row.each do |pixel|
      print pixel ? '#' : '.'
    end
    puts
  end
end

def process_instruction(instruction, screen)
  case instruction
  when /rect (\d+)x(\d+)/
    a, b = $1.to_i, $2.to_i
    rect(screen, a, b)
  when /rotate row y=(\d+) by (\d+)/
    a, b = $1.to_i, $2.to_i
    rotate_row(screen, a, b)
  when /rotate column x=(\d+) by (\d+)/
    a, b = $1.to_i, $2.to_i
    rotate_column(screen, a, b)
  end
end

def rect(screen, a, b)
  b.times do |y|
    a.times do |x|
      screen[y][x] = true
    end
  end
end

def rotate_row(screen, row, shift)
  screen[row].rotate!(-shift)
end

def rotate_column(screen, col, shift)
  column = screen.map { |row| row[col] }
  column.rotate!(-shift)
  screen.each_with_index { |row, i| row[col] = column[i] }
end

screen = Array.new(SCREEN_HEIGHT) { Array.new(SCREEN_WIDTH, false) }

File.readlines('input.txt').each do |line|
  process_instruction(line.chomp, screen)
end

display_screen(screen)