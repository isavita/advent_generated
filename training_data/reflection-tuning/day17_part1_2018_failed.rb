def parse_input(input)
  clay = Hash.new { |h, k| h[k] = {} }
  min_y, max_y = Float::INFINITY, -Float::INFINITY

  input.each_line do |line|
    a, b = line.strip.split(', ')
    if a[0] == 'x'
      x = a[2..-1].to_i
      y_range = b[2..-1].split('..').map(&:to_i)
      (y_range[0]..y_range[1]).each { |y| clay[y][x] = '#' }
      min_y = [min_y, y_range[0]].min
      max_y = [max_y, y_range[1]].max
    else
      y = a[2..-1].to_i
      x_range = b[2..-1].split('..').map(&:to_i)
      (x_range[0]..x_range[1]).each { |x| clay[y][x] = '#' }
      min_y = [min_y, y].min
      max_y = [max_y, y].max
    end
  end

  [clay, min_y, max_y]
end

def flow(clay, x, y, min_y, max_y)
  return if y > max_y
  return if clay[y][x] == '|'

  clay[y][x] = '|'

  if y < max_y && !['#', '~'].include?(clay[y + 1][x])
    flow(clay, x, y + 1, min_y, max_y)
    return if clay[y + 1][x] == '|'
  end

  left_wall = right_wall = false
  left = right = x

  loop do
    left_wall = clay[y][left - 1] == '#'
    break if left_wall || clay[y + 1][left - 1] == '|' || !['#', '~'].include?(clay[y + 1][left])
    left -= 1
  end

  loop do
    right_wall = clay[y][right + 1] == '#'
    break if right_wall || clay[y + 1][right + 1] == '|' || !['#', '~'].include?(clay[y + 1][right])
    right += 1
  end

  if left_wall && right_wall
    (left..right).each { |i| clay[y][i] = '~' }
  else
    flow(clay, left - 1, y, min_y, max_y) unless left_wall
    flow(clay, right + 1, y, min_y, max_y) unless right_wall
  end
end

def count_water(clay, min_y, max_y)
  clay.sum do |y, row|
    next 0 if y < min_y || y > max_y
    row.count { |_, tile| ['~', '|'].include?(tile) }
  end
end

clay, min_y, max_y = parse_input(ARGF.read)
flow(clay, 500, 0, min_y, max_y)
puts count_water(clay, min_y, max_y)
