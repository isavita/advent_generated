def is_prime?(n)
  return false if n <= 1
  return true if n == 2
  return false if n.even?
  
  3.step(Math.sqrt(n).to_i, 2) do |i|
    return false if n % i == 0
  end
  true
end

start = 107900
end_num = 124900
step = 17

count = 0
(start..end_num).step(step) do |num|
  count += 1 unless is_prime?(num)
end

puts count
