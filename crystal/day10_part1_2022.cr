
def read_all(path : String) : String
  File.read(path)
end

x = [1]
File.read("input.txt").lines.each do |line|
  case line
  when "noop"
    x << x.last
  else
    n = line.split(" ").last.to_i
    x << x.last
    x << x.last + n
  end
end

sum = 0
x.each_with_index do |val, i|
  if (i - 19) % 40 == 0
    sum += (i + 1) * val
  end
end

puts sum
