def main
  input = File.read("input.txt").strip
  tiles = parse_tiles_from_input(input)
  edge_size = Math.sqrt(tiles.size).to_i

  assembled_tiles = backtrack_assemble(tiles, nil, {})

  assembled_tiles.each_with_index do |row, r|
    row.each_with_index do |cell, c|
      assembled_tiles[r][c][:contents] = remove_borders_from_grid(cell[:contents])
    end
  end

  image = []
  edge_size.times do |big_row|
    assembled_tiles[0][0][:contents].size.times do |sub_row|
      image << []
      edge_size.times do |big_col|
        sub_line = assembled_tiles[big_row][big_col][:contents][sub_row]
        image.last.concat(sub_line)
      end
    end
  end

  monster_coords = []
  all_grid_orientations(image).each do |opt|
    monster_coords = find_monster_coords(opt)
    break if monster_coords.any?
  end

  monster_coords.each { |coord| image[coord[0]][coord[1]] = 'O' }

  rough_waters_count = image.sum { |row| row.count('#') }

  puts rough_waters_count
end

def parse_tiles_from_input(input)
  input.split("\n\n").map do |block|
    lines = block.split("\n")
    tile_id = lines[0].match(/\d+/)[0].to_i
    contents = lines[1..].map { |line| line.chars }
    { id: tile_id, contents: contents }
  end
end

def backtrack_assemble(tiles, assembled_tiles, used_indices)
  edge_size = Math.sqrt(tiles.size).to_i
  assembled_tiles ||= Array.new(edge_size) { Array.new(edge_size) }

  edge_size.times do |row|
    edge_size.times do |col|
      next if assembled_tiles[row][col]

      tiles.each_with_index do |t, i|
        next if used_indices[i]

        all_grid_orientations(t[:contents]).each do |opt|
          if (row > 0 && get_row(opt, true) != get_row(assembled_tiles[row-1][col][:contents], false)) ||
             (col > 0 && get_col(opt, true) != get_col(assembled_tiles[row][col-1][:contents], false))
            next
          end

          t[:contents] = opt
          assembled_tiles[row][col] = t
          used_indices[i] = true
          result = backtrack_assemble(tiles, assembled_tiles, used_indices)
          return result if result

          assembled_tiles[row][col] = nil
          used_indices.delete(i)
        end
      end

      return nil unless assembled_tiles[row][col]
    end
  end

  assembled_tiles
end

def get_col(grid, first_col)
  (0...grid.size).map { |i| grid[i][first_col ? 0 : -1] }.join
end

def get_row(grid, first_row)
  (0...grid[0].size).map { |i| grid[first_row ? 0 : -1][i] }.join
end

def remove_borders_from_grid(grid)
  grid[1...-1].map { |row| row[1...-1] }
end

def find_monster_coords(image)
  monster = "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "
  monster_offsets = []
  monster.split("\n").each_with_index do |line, r|
    line.chars.each_with_index do |char, c|
      monster_offsets << [r, c] if char == '#'
    end
  end

  monster_coords = []
  (0..image.size-monster_offsets.max[0]-1).each do |r|
    (0..image[0].size-monster_offsets.max[1]-1).each do |c|
      monster_found = monster_offsets.all? { |dr, dc| image[r+dr][c+dc] == '#' }
      monster_offsets.each { |dr, dc| monster_coords << [r+dr, c+dc] } if monster_found
    end
  end

  monster_coords.uniq
end

def all_grid_orientations(grid)
  orientations = [grid]
  3.times { orientations << rotate_string_grid(orientations.last) }
  orientations += orientations.map { |g| mirror_string_grid(g) }
end

def rotate_string_grid(grid)
  Array.new(grid[0].size) { |i| Array.new(grid.size) { |j| grid[j][grid[0].size-1-i] } }
end

def mirror_string_grid(grid)
  grid.map(&:reverse)
end

main