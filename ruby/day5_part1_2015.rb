
input = File.read('input.txt').strip

nice = input.lines.count do |line|
  line.count('aeiou') >= 3 &&
  line =~ /(.)\1/ &&
  !(line =~ /ab|cd|pq|xy/)
end

puts nice
