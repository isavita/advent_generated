
input = File.read('input.txt').strip
floor = 0
position = input.each_char.with_index(1) do |c, i|
  floor += (c == '(' ? 1 : -1)
  break i if floor == -1
end
puts position
