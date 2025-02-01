
MOD = 1 << 24
NUM_STEPS = 2000

def next_secret(s)
  x = s * 64
  s ^= x
  s &= MOD - 1
  x = s / 32
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

initials = File.readlines("input.txt").map(&:to_i)

buyers = initials.map do |init_val|
  prices = []
  s = init_val
  (NUM_STEPS + 1).times do |j|
    prices << (s % 10)
    s = next_secret(s) if j < NUM_STEPS
  end
  changes = (0...NUM_STEPS).map { |j| prices[j+1] - prices[j] }
  { prices: prices, changes: changes }
end

pattern_count = 19**4
global_sum = Array.new(pattern_count, 0)

buyers.each do |b|
  local_price = Array.new(pattern_count, -1)
  (0...NUM_STEPS - 3).each do |i|
    c1, c2, c3, c4 = b[:changes][i], b[:changes][i+1], b[:changes][i+2], b[:changes][i+3]
    next if c1 < -9 || c1 > 9 || c2 < -9 || c2 > 9 || c3 < -9 || c3 > 9 || c4 < -9 || c4 > 9
    idx = encode_change4(c1, c2, c3, c4)
    local_price[idx] = b[:prices][i+4] if local_price[idx] < 0
  end
  local_price.each_with_index { |p, idx| global_sum[idx] += p if p >= 0 }
end

ans = global_sum.max
puts ans
