
def solve
  grid = File.readlines("input.txt").map { |line| line.strip.chars.map(&:to_i) }
  nr = grid.size
  nc = grid[0].size
  dp = Array.new(nr) { Array.new(nc, -1) }
  dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]

  dfs = lambda do |r, c|
    return dp[r][c] if dp[r][c] != -1
    h = grid[r][c]
    if h == 9
      dp[r][c] = 1
      return 1
    end
    sum = 0
    dirs.each do |dr, dc|
      nr2, nc2 = r + dr, c + dc
      next if nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc
      sum += dfs.call(nr2, nc2) if grid[nr2][nc2] == h + 1
    end
    dp[r][c] = sum
    sum
  end

  total = 0
  nr.times do |r|
    nc.times do |c|
      total += dfs.call(r, c) if grid[r][c] == 0
    end
  end
  puts total
end

solve
