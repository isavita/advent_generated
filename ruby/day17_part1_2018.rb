require 'stringio'

def str_to_int(s)
  s.to_i
end

def reg_split(text, delimeter)
  text.split(Regexp.new(delimeter))
end

def main
  input = File.read('input.txt')
  input_str = input.strip
  lines = reg_split(input_str, "\n")

  ground = [['+']]
  max_x, min_x, max_y, min_y = 0, 0, 0, 20
  x_offset, y_offset = 500, 0

  lines.each do |line|
    split = reg_split(line, "[=, .]+")
    if split[0] == "x"
      x = str_to_int(split[1]) - x_offset
      y1 = str_to_int(split[3]) - y_offset
      y2 = str_to_int(split[4]) - y_offset

      while x >= max_x
        max_x += 1
        ground.each { |row| row << '.' }
      end
      while x <= min_x
        min_x -= 1
        ground.each { |row| row.unshift('.') }
      end
      while y2 > max_y
        max_y += 1
        ground << Array.new(ground[0].length, '.')
      end
      min_y = y1 if y1 < min_y
      (y1..y2).each { |i| ground[i][x - min_x] = '#' }
    else
      y = str_to_int(split[1]) - y_offset
      x1 = str_to_int(split[3]) - x_offset
      x2 = str_to_int(split[4]) - x_offset

      while y > max_y
        max_y += 1
        ground << Array.new(ground[0].length, '.')
      end
      while x2 >= max_x
        max_x += 1
        ground.each { |row| row << '.' }
      end
      while x1 <= min_x
        min_x -= 1
        ground.each { |row| row.unshift('.') }
      end
      (x1..x2).each { |i| ground[y][i - min_x] = '#' }
      min_y = y if y < min_y
    end
  end

  water_count = 0
  flow_count = 0
  round_limit = 200_000

  while ground[1][-min_x] != '|' && water_count < round_limit
    can_move = true
    x = -min_x
    y = 1
    try_left = 0
    while can_move
      if y + 1 > max_y || ground[y + 1][x] == '|'
        ground[y][x] = '|'
        can_move = false
        flow_count += 1 if y >= min_y
      elsif ground[y + 1][x] == '.'
        y += 1
        try_left = 0
      elsif ground[y + 1][x] == '#' || ground[y + 1][x] == '~'
        if ((try_left == 1 && ground[y][x - 1] == '|') ||
            (try_left == 2 && ground[y][x + 1] == '|') ||
            (ground[y][x + 1] == '|' && ground[y][x - 1] != '.') ||
            (ground[y][x + 1] != '.' && ground[y][x - 1] == '|'))
          ground[y][x] = '|'
          flow_count += 1
          can_move = false
          (x + 1).upto(ground[y].length - 1) do |i|
            break if ground[y][i] != '~'
            ground[y][i] = '|'
            water_count -= 1
            flow_count += 1
          end
          (x - 1).downto(0) do |i|
            break if ground[y][i] != '~'
            ground[y][i] = '|'
            water_count -= 1
            flow_count += 1
          end
        elsif ((try_left == 0 && ground[y][x - 1] == '.') ||
              (try_left == 1 && ground[y][x - 1] == '.'))
          x -= 1
          try_left = 1
        elsif ((try_left == 0 && ground[y][x + 1] == '.') ||
              (try_left == 2 && ground[y][x + 1] == '.'))
          x += 1
          try_left = 2
        else
          can_move = false
          ground[y][x] = '~'
          water_count += 1
        end
      end
    end
  end

  puts flow_count + water_count
end

main