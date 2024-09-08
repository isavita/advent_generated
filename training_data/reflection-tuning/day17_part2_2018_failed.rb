class WaterFlow
  def initialize(clay_veins)
    @grid = {}
    @min_x, @max_x, @min_y, @max_y = Float::INFINITY, -Float::INFINITY, Float::INFINITY, -Float::INFINITY
    parse_clay_veins(clay_veins)
    @spring = [500, 0]
  end

  def parse_clay_veins(clay_veins)
    clay_veins.each do |vein|
      parts = vein.split(', ')
      x_part, y_part = parts[0], parts[1]
      
      x_range = parse_range(x_part)
      y_range = parse_range(y_part)
      
      x_range.each do |x|
        y_range.each do |y|
          @grid[[x, y]] = '#'
          @min_x = [x, @min_x].min
          @max_x = [x, @max_x].max
          @min_y = [y, @min_y].min
          @max_y = [y, @max_y].max
        end
      end
    end
  end

  def parse_range(part)
    if part.include?('..')
      start, finish = part.split('=')[1].split('..').map(&:to_i)
      (start..finish)
    else
      value = part.split('=')[1].to_i
      (value..value)
    end
  end

  def flow
    queue = [@spring]
    while !queue.empty?
      x, y = queue.shift
      next if y > @max_y

      if @grid[[x, y]] != '#' && y <= @max_y
        @grid[[x, y]] = '|'
        if can_fall?(x, y)
          queue << [x, y + 1]
        else
          left_bound = flow_sideways(x, y, -1)
          right_bound = flow_sideways(x, y, 1)
          if left_bound && right_bound
            (left_bound..right_bound).each { |xx| @grid[[xx, y]] = '~' }
            queue << [x, y - 1]
          else
            queue << [x - 1, y] if !left_bound
            queue << [x + 1, y] if !right_bound
          end
        end
      end
    end
  end

  def can_fall?(x, y)
    @grid[[x, y + 1]] != '#' && @grid[[x, y + 1]] != '~'
  end

  def flow_sideways(x, y, dx)
    while @grid[[x + dx, y]] != '#' && !can_fall?(x + dx, y)
      x += dx
      @grid[[x, y]] = '|'
    end
    @grid[[x + dx, y]] == '#' ? x : nil
  end

  def count_water
    @grid.count { |_, v| v == '|' || v == '~' }
  end

  def count_standing_water
    @grid.count { |_, v| v == '~' }
  end
end

clay_veins = File.readlines('input.txt').map(&:strip)
water_flow = WaterFlow.new(clay_veins)
water_flow.flow

puts "Part 1: #{water_flow.count_water}"
puts "Part 2: #{water_flow.count_standing_water}"
