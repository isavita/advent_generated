def solve_part2
  b = 57 * 100 + 100000
  c = b + 17000
  h = 0

  while b <= c
    f = 1
    d = 2
    while d * d <= b
      if b % d == 0
        f = 0
        break
      end
      d += 1
    end
    h += 1 if f == 0
    b += 17
  end

  h
end

puts solve_part2
