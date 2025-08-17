
MAX_DIM = 200
PADDING = 1
GRID_DIM = MAX_DIM + 2 * PADDING

struct Point
  property r : Int32
  property c : Int32
  def initialize(@r : Int32, @c : Int32); end
end

grid = Array.new(GRID_DIM) { Array.new(GRID_DIM, '.' ) }
visited = Array.new(GRID_DIM) { Array.new(GRID_DIM, false) }
is_in_region = Array.new(GRID_DIM) { Array.new(GRID_DIM, false) }

lines = File.read_lines("input.txt")
height = 0
width = 0

lines.each do |line|
  next if line.empty?
  width = line.size if width == 0
  line.each_char.with_index do |ch, i|
    grid[height + PADDING][i + PADDING] = ch
  end
  height += 1
end

if height == 0 || width == 0
  abort "empty input"
end

def exposed?(grid, r, c, plant)
  grid[r][c] != plant
end

total1 = 0_i64
total2 = 0_i64
queue = Array(Point).new(GRID_DIM * GRID_DIM)
region_coords = Array(Point).new(GRID_DIM * GRID_DIM)

(0...height).each do |r_start|
  (0...width).each do |c_start|
    r = r_start + PADDING
    c = c_start + PADDING
    next if visited[r][c]

    plant = grid[r][c]
    area = 0_i64
    perimeter = 0_i64
    region_size = 0

    q_head = 0
    q_tail = 0
    queue << Point.new(r, c)
    q_tail += 1
    visited[r][c] = true

    while q_head < q_tail
      p = queue[q_head]
      q_head += 1
      area += 1
      region_coords << p
      region_size += 1

      [{-1, 0}, {1, 0}, {0, -1}, {0, 1}].each do |dr, dc|
        nr = p.r + dr
        nc = p.c + dc
        if grid[nr][nc] != plant
          perimeter += 1
        elsif !visited[nr][nc]
          visited[nr][nc] = true
          queue << Point.new(nr, nc)
          q_tail += 1
        end
      end
    end

    total1 += area * perimeter

    top = bottom = left = right = 0_i64
    top_adj = bottom_adj = left_adj = right_adj = 0_i64

    (0...GRID_DIM).each { |i| is_in_region[i].fill(false) }
    region_coords[0, region_size].each { |p| is_in_region[p.r][p.c] = true }

    region_coords[0, region_size].each do |p|
      top += 1_i64 if exposed?(grid, p.r - 1, p.c, plant)
      bottom += 1_i64 if exposed?(grid, p.r + 1, p.c, plant)
      left += 1_i64 if exposed?(grid, p.r, p.c - 1, plant)
      right += 1_i64 if exposed?(grid, p.r, p.c + 1, plant)

      if is_in_region[p.r][p.c + 1]
        top_adj += 1_i64 if exposed?(grid, p.r - 1, p.c, plant) && exposed?(grid, p.r - 1, p.c + 1, plant)
        bottom_adj += 1_i64 if exposed?(grid, p.r + 1, p.c, plant) && exposed?(grid, p.r + 1, p.c + 1, plant)
      end
      if is_in_region[p.r + 1][p.c]
        left_adj += 1_i64 if exposed?(grid, p.r, p.c - 1, plant) && exposed?(grid, p.r + 1, p.c - 1, plant)
        right_adj += 1_i64 if exposed?(grid, p.r, p.c + 1, plant) && exposed?(grid, p.r + 1, p.c + 1, plant)
      end
    end

    sides = (top - top_adj) + (bottom - bottom_adj) + (left - left_adj) + (right - right_adj)
    total2 += area * sides

    queue.clear
    region_coords.clear
  end
end

puts "Part 1 Total Price: #{total1}"
puts "Part 2 Total Price: #{total2}"
