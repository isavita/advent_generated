
line = File.read("input.txt").strip
disk = []
file_id = 0
is_file = true
line.each_char do |char|
  length = char.to_i
  if is_file
    length.times { disk << file_id.to_s }
    file_id += 1
  else
    length.times { disk << '.' }
  end
  is_file = !is_file
end

loop do
  lfree = disk.index('.')
  break unless lfree
  rfile = disk.rindex { |x| x != '.' }
  break unless rfile && rfile > lfree
  disk[lfree], disk[rfile] = disk[rfile], '.'
end

checksum = 0
disk.each_with_index do |b, i|
  if b != '.'
    checksum += i * b.to_i
  end
end
puts checksum
