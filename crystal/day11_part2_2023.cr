
def solve(expansion : Int64)
  image = File.read_lines("input.txt").map(&.chomp).to_a
  rows = image.size
  cols = image[0].size

  empty_rows = (0...rows).to_a.select { |r| image[r].chars.all? { |c| c == '.' } }
  empty_cols = (0...cols).to_a.select { |c| (0...rows).all? { |r| image[r][c] == '.' } }

  galaxies = [] of Tuple(Int64, Int64)
  (0...rows).each do |r|
    (0...cols).each do |c|
      if image[r][c] == '#'
        galaxies << {r.to_i64, c.to_i64}
      end
    end
  end

  total_distance = 0_i64
  (0...galaxies.size).each do |i|
    (i+1...galaxies.size).each do |j|
      r1, c1 = galaxies[i]
      r2, c2 = galaxies[j]

      row_expansion = empty_rows.count { |r| (r1 < r && r < r2) || (r2 < r && r < r1) }.to_i64
      col_expansion = empty_cols.count { |c| (c1 < c && c < c2) || (c2 < c && c < c1) }.to_i64

      distance = (r1 - r2).abs + (c1 - c2).abs + row_expansion * (expansion - 1) + col_expansion * (expansion - 1)
      total_distance += distance
    end
  end

  puts total_distance
end


def main
  solve(2)
  solve(1_000_000)
end

main
