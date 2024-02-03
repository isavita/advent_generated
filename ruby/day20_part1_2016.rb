
blacklist = File.readlines('input.txt').map { |line| line.split('-').map(&:to_i) }
blacklist.sort!

current = 0
blacklist.each do |range|
  if current < range[0]
    puts current
    break
  else
    current = [current, range[1] + 1].max
  end
end
