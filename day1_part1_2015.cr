file = File.read("input.txt")
input = file.strip
floor = 0

input.each_char do |c|
  if c == '('
    floor += 1
  elsif c == ')'
    floor -= 1
  end
end

puts floor
