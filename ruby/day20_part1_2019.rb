
def neighbours(x, y)
  [[x, y + 1], [x + 1, y], [x, y - 1], [x - 1, y]]
end

def parse()
  grid = {}
  x_max = 0
  y_max = 0
  file = File.open('input.txt')
  i = 0
  file.each_line do |line|
    y_max = line.length if line.length > y_max
    line.strip.each_char.with_index do |c, j|
      grid[[i, j]] = c
    end
    i += 1
  end
  x_max = i
  aa = nil
  zz = nil
  is_outer = {}
  portal_name = {}
  teleport = {}
  cache = {}

  (0...x_max).each do |i|
    (0...y_max).each do |j|
      c = grid[[i, j]]
      next unless c.match?(/[A-Z]/)
      p_name, p_point, ok = extract_portal(grid, i, j)
      next unless ok
      portal_name[p_point] = p_name

      if p_name == 'AA'
        aa = p_point
        is_outer[p_point] = true
        next
      end

      if p_name == 'ZZ'
        zz = p_point
        is_outer[p_point] = true
        next
      end
      if cache.key?(p_name)
        teleport[p_point] = cache[p_name]
        teleport[cache[p_name]] = p_point
      else
        cache[p_name] = p_point
      end

      if j == 0 || i == 0 || i == x_max - 2 || j == y_max - 2
        is_outer[p_point] = true
      else
        is_outer[p_point] = false
      end
    end
  end

  return grid, aa, zz, teleport, x_max, y_max
end

def extract_portal(grid, i, j)
  c1 = grid[[i, j]]
  if grid[[i + 1, j]].match?(/[A-Z]/)
    c2 = grid[[i + 1, j]]
    portal_name = c1 + c2
    portal_point = [i + 2, j]
    return portal_name, portal_point, true if grid[portal_point] == '.'
    portal_point = [i - 1, j]
    return portal_name, portal_point, true if grid[portal_point] == '.'
  end

  if grid[[i, j + 1]].match?(/[A-Z]/)
    c2 = grid[[i, j + 1]]
    portal_name = c1 + c2
    portal_point = [i, j + 2]
    return portal_name, portal_point, true if grid[portal_point] == '.'
    portal_point = [i, j - 1]
    return portal_name, portal_point, true if grid[portal_point] == '.'
  end
  ['', [], false]
end

def bfs(grid, aa, zz, teleport)
  discovered = {}
  to_do = []
  discovered[aa] = true
  to_do << aa
  depth = 0

  until to_do.empty?
    to_do.size.times do
      curr = to_do.shift
      return depth if curr == zz
      neighbours(curr[0], curr[1]).each do |n|
        dest = grid[n]
        case
        when dest == '#'
          next
        when dest == '.'
          unless discovered.key?(n)
            discovered[n] = true
            to_do << n
          end
        when dest.match?(/[A-Z]/)
          nxt = teleport[curr]
          unless discovered.key?(nxt)
            discovered[nxt] = true
            to_do << nxt
          end
        end
      end
    end
    depth += 1
  end
  -1
end

grid, aa, zz, teleport, x_max, y_max = parse()
puts bfs(grid, aa, zz, teleport)
