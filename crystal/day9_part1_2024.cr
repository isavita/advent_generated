
def main
  line = File.read("input.txt").lines.first?.try(&.strip) || ""
  disk = [] of Int64
  file_id = 0_i64
  is_file = true
  line.each_char do |ch|
    len = ch.to_i
    if is_file
      disk.concat(Array.new(len, file_id))
      file_id += 1
    else
      disk.concat(Array.new(len, -1_i64))
    end
    is_file = !is_file
  end

  loop do
    lfree = -1
    disk.each_with_index do |v, i|
      if v == -1
        lfree = i
        break
      end
    end
    break if lfree == -1

    rfile = -1
    (disk.size - 1).downto(lfree + 1) do |i|
      if disk[i] != -1
        rfile = i
        break
      end
    end
    break if rfile == -1

    disk[lfree] = disk[rfile]
    disk[rfile] = -1
  end

  checksum = 0_i64
  disk.each_with_index do |v, i|
    checksum += i.to_i64 * v if v != -1
  end
  puts checksum
end

main
