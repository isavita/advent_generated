
def solve
  grid = File.readlines("input.txt").map { |line| line.strip.chars.map(&:to_i) }
  nr = grid.size
  nc = grid[0].size
  dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]
  trailheads = []
  nr.times { |r| nc.times { |c| trailheads << [r, c] if grid[r][c] == 0 } }

  sum_scores = 0
  trailheads.each do |th|
    reached = {}
    front = [[th, 0]]
    visited = {}
    while front.any?
      cur_p, cur_h = front.pop
      if cur_h == 9
        reached[cur_p] = true unless reached[cur_p]
        next
      end
      dirs.each do |dr, dc|
        nr2, nc2 = cur_p[0] + dr, cur_p[1] + dc
        next if nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc
        if grid[nr2][nc2] == cur_h + 1
          key = [nr2, nc2, cur_h + 1]
          unless visited[key]
            visited[key] = true
            front << [[nr2, nc2], cur_h + 1]
          end
        end
      end
    end
    sum_scores += reached.size
  end
  puts sum_scores
end

solve
