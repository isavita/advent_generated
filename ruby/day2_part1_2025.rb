
def ceil_div(a, b)
  (a + b - 1) / b
end

data = File.read('input.txt').gsub(/\r?\n/, '').strip
ranges = data.split(',').map { |p| s, e = p.split('-'); [s.to_i, e.to_i] }

total = 0

(1..10).each do |k|
  mul = 10**k + 1
  min_seed = 10**(k - 1)
  max_seed = 10**k - 1

  ranges.each do |st, en|
    s_min = ceil_div(st, mul)
    s_max = en / mul

    from = [s_min, min_seed].max
    to   = [s_max, max_seed].min

    if from <= to
      cnt = to - from + 1
      total += mul * (from + to) * cnt / 2
    end
  end
end

puts total
