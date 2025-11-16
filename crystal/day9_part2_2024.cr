
disk = [] of Int32
file_id = 0
is_file = true
File.read("input.txt").chomp.chars.each do |c|
  len = c.to_i
  if is_file
    len.times { disk << file_id }
    file_id += 1
  else
    len.times { disk << -1 }
  end
  is_file = !is_file
end

files = [] of {id: Int32, start: Int32, end: Int32}
cur_id = -1
start = 0
disk.size.times do |i|
  next if disk[i] == -1
  id = disk[i]
  if id != cur_id
    cur_id = id
    start = i
  end
  if i == disk.size - 1 || (i + 1 < disk.size && disk[i + 1] != id)
    files << {id: id, start: start, end: i}
  end
end

files.reverse_each do |f|
  file_len = f[:end] - f[:start] + 1
  leftmost_span = -1
  span_len = 0
  f[:start].times do |j|
    if disk[j] == -1
      leftmost_span = j if span_len == 0
      span_len += 1
      break if span_len == file_len
    else
      span_len = 0
      leftmost_span = -1
    end
  end
  if leftmost_span != -1 && span_len == file_len
    (f[:start]..f[:end]).each { |x| disk[x] = -1 }
    file_len.times { |x| disk[leftmost_span + x] = f[:id] }
  end
end

checksum = 0i64
disk.size.times { |i| checksum += i * disk[i] if disk[i] != -1 }
puts checksum
