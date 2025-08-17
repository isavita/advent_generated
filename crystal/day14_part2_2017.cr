
#!/usr/bin/env crystal

def reverse_section(list : Array(Int32), pos : Int32, len : Int32, n : Int32)
  i = pos
  j = pos + len - 1
  while i < j
    a = list[i % n]
    list[i % n] = list[j % n]
    list[j % n] = a
    i += 1
    j -= 1
  end
end

def knot_hash(input : String) : String
  lengths = input.bytes.map(&.to_i32)
  lengths += [17, 31, 73, 47, 23]
  list = Array(Int32).new(256) { |i| i.to_i32 }
  pos = 0_i32
  skip = 0_i32
  64.times do
    lengths.each do |len|
      reverse_section(list, pos, len, 256)
      pos += len + skip
      skip += 1
    end
  end
  dense = Array(Int32).new(16) { 0 }
  16.times do |i|
    xor = 0
    16.times { |j| xor ^= list[i * 16 + j] }
    dense[i] = xor
  end
  dense.map { |x| "%02x" % x }.join
end

def hex_to_bits(hex : String) : Array(Int32)
  bits = Array(Int32).new(128) { 0 }
  hex.chars.each_with_index do |c, idx|
    val = c.ord <= 57 ? c.ord - 48 : c.ord - 87
    4.times do |b|
      bits[idx * 4 + b] = ((val >> (3 - b)) & 1)
    end
  end
  bits
end

def dfs(x : Int32, y : Int32, grid : Array(Array(Int32)))
  return if x < 0 || y < 0 || x >= 128 || y >= 128
  return if grid[x][y] == 0
  grid[x][y] = 0
  dfs(x - 1, y, grid)
  dfs(x + 1, y, grid)
  dfs(x, y - 1, grid)
  dfs(x, y + 1, grid)
end

key = File.read("input.txt").strip
grid = Array(Array(Int32)).new(128) { Array(Int32).new(128, 0) }
total_used = 0

128.times do |i|
  row_key = "#{key}-#{i}"
  hash = knot_hash(row_key)
  bits = hex_to_bits(hash)
  bits.each_with_index do |bit, j|
    if bit == 1
      grid[i][j] = 1
      total_used += 1
    end
  end
end

regions = 0
128.times do |i|
  128.times do |j|
    if grid[i][j] == 1
      regions += 1
      dfs(i, j, grid)
    end
  end
end

puts regions
