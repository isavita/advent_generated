
def next_secret(s)
  x = s * 64
  s ^= x
  s &= 0xFFFFFF
  x = s / 32
  s ^= x
  s &= 0xFFFFFF
  x = s * 2048
  s ^= x
  s &= 0xFFFFFF
  s
end

total = File.readlines("input.txt").compact.map(&:to_i).sum do |buyer|
  s = buyer
  2000.times { s = next_secret(s) }
  s
end

puts total
