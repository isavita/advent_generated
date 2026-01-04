
require "big"

def sum_invalid_ids_in_range(l : BigInt, r : BigInt) : BigInt
  sum = BigInt.zero
  (1..50).each do |k|
    pow10 = (10.to_big_i ** k)
    mult = pow10 + 1.to_big_i
    min_s = 10.to_big_i ** (k - 1)
    max_s = pow10 - 1.to_big_i

    s_min = (l + mult - 1) // mult
    s_max = r // mult

    start = s_min > min_s ? s_min : min_s
    finish = s_max < max_s ? s_max : max_s

    if start <= finish
      cnt = finish - start + 1
      sum_s = cnt * (start + finish) // 2
      sum += sum_s * mult
    end
  end
  sum
end

def main
  path = "input.txt"
  return unless File.exists?(path)

  content = File.read(path).gsub(/\s/, "")
  return if content.empty?

  total = BigInt.zero
  content.split(',').each do |rng|
    next if rng.empty?
    a, b = rng.split('-')
    next unless a && b
    low = BigInt.new(a)
    high = BigInt.new(b)
    total += sum_invalid_ids_in_range(low, high)
  end

  puts total
end

main
