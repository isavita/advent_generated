
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

files = []
cur_id = nil
start = nil
disk.each_with_index do |b, i|
  if b == '.'
    cur_id = nil
    next
  end
  id = b.to_i
  if id != cur_id
    cur_id = id
    start = i
  end
  if i == disk.size - 1 || (i + 1 < disk.size && disk[i + 1].to_i != id)
    files << [id, start, i]
  end
end

files.reverse_each do |id, start, ending|
  file_len = ending - start + 1
  leftmost_span = -1
  span_len = 0
  (0...start).each do |j|
    if disk[j] == '.'
      if span_len == 0
        leftmost_span = j
      end
      span_len += 1
      break if span_len == file_len
    else
      span_len = 0
      leftmost_span = -1
    end
  end
  if leftmost_span != -1 && span_len == file_len
    (start..ending).each { |x| disk[x] = '.' }
    file_len.times { |x| disk[leftmost_span + x] = id.to_s }
  end
end

checksum = 0
disk.each_with_index do |b, i|
  if b != '.'
    checksum += i * b.to_i
  end
end
puts checksum
