
def main
  rows = File.readlines('input.txt').map { |l| l.chomp.chars }
  unless rows && !rows.empty?
    puts 'Total rolls removed: 0'
    return
  end

  r_cnt = rows.size
  c_cnt = rows[0].size
  dirs  = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
  total = 0

  loop do
    rem = []
    r_cnt.times do |r|
      c_cnt.times do |c|
        next unless rows[r][c] == '@'
        n = 0
        dirs.each do |dr, dc|
          nr = r + dr
          nc = c + dc
          n += 1 if nr.between?(0, r_cnt - 1) && nc.between?(0, c_cnt - 1) && rows[nr][nc] == '@'
        end
        rem << [r, c] if n < 4
      end
    end
    break if rem.empty?
    total += rem.size
    rem.each { |r, c| rows[r][c] = '.' }
  end

  puts "Total rolls removed: #{total}"
end

main if __FILE__ == $0
