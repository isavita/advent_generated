
def invalid?(x : UInt64) : Bool
  s = x.to_s
  n = s.size
  return false if n <= 1
  (1..n // 2).each do |p|
    next unless n % p == 0
    k = n // p
    next if k < 2
    ok = true
    (p...n).each do |i|
      if s[i] != s[i % p]
        ok = false
        break
      end
    end
    return true if ok
  end
  false
end

sum = 0_u64
content = File.read("input.txt")
content.scan(/(\d+)-(\d+)/) do |m|
  a = m[1].to_u64
  b = m[2].to_u64
  a, b = b, a if a > b
  x = a
  loop do
    sum += x if invalid?(x)
    break if x == b || x == UInt64::MAX
    x += 1
  end
end

puts sum
