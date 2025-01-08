
def next_secret(s : UInt64) : UInt64
  x = s * 64
  s ^= x
  s &= 0xFFFFFF
  x = s // 32
  s ^= x
  s &= 0xFFFFFF
  x = s * 2048
  s ^= x
  s &= 0xFFFFFF
  s
end

total = File.open("input.txt") do |file|
  file.each_line.sum(0_u64) do |line|
    next 0_u64 if line.empty?
    b = line.to_u64
    s = b
    2000.times { s = next_secret(s) }
    s
  end
end

puts total
