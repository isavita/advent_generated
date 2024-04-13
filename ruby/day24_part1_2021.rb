def main
  k, l, m = [], [], []
  File.read("input.txt").split("\n").each_with_index do |line, i|
    case i % 18
    when 4
      l << line.split.last.to_i
    when 5
      k << line.split.last.to_i
    when 15
      m << line.split.last.to_i
    end
  end

  constraints = {}
  stack = []
  l.each_with_index do |value, i|
    if value == 1
      stack.push(i)
    elsif value == 26
      pop = stack.pop
      constraints[pop] = [i, m[pop] + k[i]]
    end
  end

  max = Array.new(14, 0)
  constraints.each do |i, (j, diff)|
    vmax = 9
    vmax -= 1 while vmax + diff > 9
    max[i] = vmax
    max[j] = vmax + diff
  end

  puts max.join.to_i
end

main