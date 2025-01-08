
def parse_lock(b)
  h = Array.new(5, 0)
  5.times do |c|
    cnt = 0
    6.times do |r|
      if b[r + 1][c] == '#'
        cnt += 1
      else
        break
      end
    end
    h[c] = cnt
  end
  h
end

def parse_key(b)
  h = Array.new(5, 0)
  5.times do |c|
    cnt = 0
    5.downto(0) do |r|
      if b[r][c] == '#'
        cnt += 1
      else
        break
      end
    end
    h[c] = cnt
  end
  h
end

def fits(lock, key)
  5.times do |i|
    return false if lock[i] + key[i] > 5
  end
  true
end

raw = [] of String
File.each_line("input.txt") do |line|
  line = line.strip
  raw << line unless line.empty?
end

if raw.size % 7 != 0
  puts "0"
  exit
end

locks = [] of Array(Int32)
keys = [] of Array(Int32)

(0..raw.size - 1).step(7) do |i|
  block = raw[i, 7]
  valid = true
  block.each do |ln|
    if ln.size < 5
      valid = false
      break
    end
  end
  next unless valid
  if block[0].chars.all?('#')
    locks << parse_lock(block)
  else
    keys << parse_key(block)
  end
end

count = 0
locks.each do |lock|
  keys.each do |key|
    count += 1 if fits(lock, key)
  end
end

puts count
