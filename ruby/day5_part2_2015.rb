
nice_strings = 0
naughty_strings = 0

File.open("input.txt").each do |line|
  line.chomp!
  
  if line.scan(/[aeiou]/).length >= 3 && line.match(/(\w)\1/) && !line.match(/ab|cd|pq|xy/)
    nice_strings += 1
  end
  
  if line.match(/(\w{2}).*\1/) && line.match(/(\w).\1/)
    naughty_strings += 1
  end
end

puts nice_strings
