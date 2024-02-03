
def find_time(discs)
  time = 0
  loop do
    break if discs.each_with_index.all? { |(positions, start), i| (start + time + i + 1) % positions == 0 }
    time += 1
  end
  time
end

discs = File.read('input.txt').scan(/Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+)./).map { |positions, start| [positions.to_i, start.to_i] }
puts find_time(discs)
discs << [11, 0]
puts find_time(discs)
