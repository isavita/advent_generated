require "big"

def read_all(path)
  File.read(path).strip
end

def num(w)
  w.map { |x| x.to_s }.join.to_i64
end

def constraints(l, m, k)
  constraints = {} of Int32 => Tuple(Int32, Int32)
  stack = [] of Int32
  l.each_with_index do |l_val, i|
    case l_val
    when 1
      stack << i
    when 26
      pop = stack.pop
      constraints[pop] = {i, m[pop] + k[i]}
    end
  end
  constraints
end

def max(constraints)
  max = [0] * 14
  constraints.each do |k, v|
    vmax = 9
    while vmax + v[1] > 9
      vmax -= 1
    end
    max[k] = vmax
    max[v[0]] = vmax + v[1]
  end
  max
end

def main
  input = read_all("input.txt")
  lines = input.split("\n")
  l = [] of Int32
  m = [] of Int32
  k = [] of Int32
  lines.each_with_index do |line, i|
    case i % 18
    when 4
      l << line.split(" ")[-1].to_i
    when 5
      k << line.split(" ")[-1].to_i
    when 15
      m << line.split(" ")[-1].to_i
    end
  end
  constraints = constraints(l, m, k)
  puts num(max(constraints))
end

main