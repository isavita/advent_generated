
MOD = 1 << 24
NUM_STEPS = 2000

def next_secret(s)
  x = s * 64
  s ^= x
  s &= MOD - 1
  x = s // 32
  s ^= x
  s &= MOD - 1
  x = s * 2048
  s ^= x
  s &= MOD - 1
  s
end

def encode_change4(c1, c2, c3, c4)
  (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19
end

initials = [] of UInt64
File.open("input.txt") do |f|
  f.each_line do |line|
    next if line.strip.empty?
    initials << line.to_u64
  end
end

buyers = [] of Tuple(Array(Int32), Array(Int32))
initials.each do |init_val|
  prices = [] of Int32
  s = init_val
  (NUM_STEPS + 1).times do
    prices << (s % 10).to_i32
    s = next_secret(s)
  end
  changes = [] of Int32
  NUM_STEPS.times do |j|
    changes << prices[j + 1] - prices[j]
  end
  buyers << {prices, changes}
end

pattern_count = 19 * 19 * 19 * 19
global_sum = Array.new(pattern_count, 0_i64)

buyers.each do |b|
  local_price = Array.new(pattern_count, -1)
  (0..NUM_STEPS - 4).each do |i|
    c1, c2, c3, c4 = b[1][i], b[1][i + 1], b[1][i + 2], b[1][i + 3]
    next if c1 < -9 || c1 > 9 || c2 < -9 || c2 > 9 || c3 < -9 || c3 > 9 || c4 < -9 || c4 > 9
    idx = encode_change4(c1, c2, c3, c4)
    local_price[idx] = b[0][i + 4] if local_price[idx] < 0
  end
  pattern_count.times do |idx|
    global_sum[idx] += local_price[idx].to_i64 if local_price[idx] >= 0
  end
end

ans = global_sum.max
puts ans
