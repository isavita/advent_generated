
def dfs(r, c, grid, dp, nr, nc, dirs)
  val = dp[r][c]
  return val if val != -1
  h = grid[r][c]
  if h == 9
    dp[r][c] = 1_i64
    return 1_i64
  end
  sum = 0_i64
  dirs.each do |dr, dc|
    nr2 = r + dr
    nc2 = c + dc
    next if nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc
    sum += dfs(nr2, nc2, grid, dp, nr, nc, dirs) if grid[nr2][nc2] == h + 1
  end
  dp[r][c] = sum
  sum
end

lines = File.read_lines("input.txt")
nr = lines.size
nc = lines[0].size
grid = lines.map { |l| l.chars.map(&.to_i) }
dp = Array.new(nr) { Array.new(nc, -1_i64) }
dirs = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]

total = 0_i64
nr.times do |r|
  nc.times do |c|
    total += dfs(r, c, grid, dp, nr, nc, dirs) if grid[r][c] == 0
  end
end

puts total
