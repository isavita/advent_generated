
def decode(pass : String) : Int32
  row = binary_to_int(pass[0, 7])
  column = binary_to_int(pass[7, pass.size])
  row * 8 + column
end

def binary_to_int(binary_str : String) : Int32
  result = 0
  binary_str.each_char.with_index do |char, i|
    result |= 1 << (binary_str.size - i - 1) if char == '1'
  end
  result
end

file = File.open("input.txt")
max_seat_id = 0

file.each_line do |line|
  pass = line.strip
  pass = pass.gsub("F", "0").gsub("B", "1").gsub("L", "0").gsub("R", "1")
  seat_id = decode(pass)
  max_seat_id = seat_id if seat_id > max_seat_id
end

puts max_seat_id
