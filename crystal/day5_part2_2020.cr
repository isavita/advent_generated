
def decode(pass : String) : Int32
  row = binary_to_int(pass[0, 7])
  column = binary_to_int(pass[7, pass.size])
  return row * 8 + column
end

def binary_to_int(binary_str : String) : Int32
  result = 0
  binary_str.each_char.with_index do |char, i|
    result |= 1 << (binary_str.size - i - 1) if char == '1'
  end
  return result
end

file = File.open("input.txt")
seat_ids = [] of Int32

file.each_line do |line|
  pass = line.strip
  pass = pass.gsub("F", "0").gsub("B", "1").gsub("L", "0").gsub("R", "1")
  seat_id = decode(pass)
  seat_ids << seat_id
end

seat_ids.sort!

(0..seat_ids.size-2).each do |i|
  if seat_ids[i+1] != seat_ids[i] + 1
    puts seat_ids[i] + 1
    break
  end
end
