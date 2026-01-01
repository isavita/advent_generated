dial_size = 100
pos = 50
hits = 0

File.foreach('input.txt') do |line|
  line = line.strip
  next if line.empty?
  dir = line[0]
  amount = line[1..].to_i
  if dir == 'R'
    hits += (pos + amount) / dial_size
    pos = (pos + amount) % dial_size
  else
    hits += (pos - 1).div(dial_size) - (pos - amount - 1).div(dial_size)
    pos = (pos - amount) % dial_size
  end
end

puts hits