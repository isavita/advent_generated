
def solve
  grid = {}
  x_max = 0
  y_max = 0

  File.readlines("input.txt").each_with_index do |line, i|
    x_max = i + 1
    y_max = [y_max, line.chomp.length].max
    line.chomp.chars.each_with_index do |c, j|
      grid[[i, j]] = c
    end
  end

  aa = nil
  zz = nil
  is_outer = {}
  portal_name = {}
  teleport = {}
  cache = {}

  (0...x_max).each do |i|
    (0...y_max).each do |j|
      c = grid[[i, j]]
      next unless c =~ /[A-Z]/

      p_name, p_point = extract_portal(grid, [i, j])
      next unless p_name

      portal_name[p_point] = p_name

      if p_name == "AA"
        aa = p_point
        is_outer[p_point] = true
        next
      end

      if p_name == "ZZ"
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

      is_outer[p_point] = (j == 0 || i == 0 || i == x_max - 2 || j == y_max - 2)
    end
  end

  puts bfs_nested(grid, aa, zz, teleport, portal_name, is_outer)
end

def extract_portal(grid, p)
  c1 = grid[p]

  if c2 = grid[[p[0] + 1, p[1]]]
    if c2 =~ /[A-Z]/
      portal_name = c1 + c2
      portal_point = [p[0] + 2, p[1]]
      return portal_name, portal_point if grid[portal_point] == '.'
      portal_point = [p[0] - 1, p[1]]
      return portal_name, portal_point if grid[portal_point] == '.'
    end
  end

  if c2 = grid[[p[0], p[1] + 1]]
    if c2 =~ /[A-Z]/
      portal_name = c1 + c2
      portal_point = [p[0], p[1] + 2]
      return portal_name, portal_point if grid[portal_point] == '.'
      portal_point = [p[0], p[1] - 1]
      return portal_name, portal_point if grid[portal_point] == '.'
    end
  end

  nil
end

def bfs_nested(grid, aa, zz, teleport, portal_name, is_outer)
  discovered = {}
  to_do = [[aa, 0]]
  steps = 0

  until to_do.empty?
    level_size = to_do.size
    level_size.times do
      curr_p, curr_depth = to_do.shift

      neighbors = [
        [curr_p[0], curr_p[1] + 1],
        [curr_p[0] + 1, curr_p[1]],
        [curr_p[0], curr_p[1] - 1],
        [curr_p[0] - 1, curr_p[1]]
      ]

      neighbors.each do |n|
        dest = grid[n]

        case dest
        when '#'
          next
        when '.'
          target = [n, curr_depth]
          unless discovered.key?(target)
            discovered[target] = true
            to_do << target
          end
        when /[A-Z]/
          is_outer_portal = is_outer[curr_p]
          if !is_outer_portal
            target = [teleport[curr_p], curr_depth + 1]
          else
            portal_name_curr = portal_name[curr_p]
            if curr_depth == 0
              return steps if portal_name_curr == "ZZ"
              next
            end
            next if portal_name_curr == "AA" || portal_name_curr == "ZZ"
            target = [teleport[curr_p], curr_depth - 1]
          end
          unless discovered.key?(target)
            discovered[target] = true
            to_do << target
          end
        end
      end
    end
    steps += 1
  end
  -1
end

solve
