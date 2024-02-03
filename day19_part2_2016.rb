
def josephus(n)
  p = 1
  p *= 2 while p * 2 <= n
  l = n - p
  2 * l + 1
end

def across_josephus(n)
  p1, p3 = 1, 3
  p1 *= 3 while p1 * 3 <= n
  return n if n == p1
  return n - p1 if n <= p1 * 2
  n - p1 + (n - 2 * p1) / 2
end

n = File.read("input.txt").to_i
puts josephus(n)
puts across_josephus(n)
