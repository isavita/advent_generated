
require "digest/sha1"

def reverse_section(arr : Array(Int32), start : Int32, length : Int32)
  n = arr.size
  (0...length // 2).each do |i|
    arr[(start + i) % n], arr[(start + length - 1 - i) % n] = arr[(start + length - 1 - i) % n], arr[(start + i) % n]
  end
end

def knot_hash(input : String) : String
  lengths = input.chars.map(&.ord.to_i32)
  lengths.concat([17, 31, 73, 47, 23])

  list = (0...256).to_a

  position = 0
  skip = 0
  64.times do
    lengths.each do |length|
      reverse_section(list, position, length)
      position += length + skip
      skip += 1
    end
  end

  dense_hash = Array(Int32).new(16) { |i|
    xor = 0
    16.times { |j| xor ^= list[i * 16 + j] }
    xor
  }

  hex_hash = ""
  dense_hash.each { |v| hex_hash += v.to_s(16).rjust(2, '0') }
  hex_hash
end

def hex_to_binary(hex_str : String) : String
  binary_str = ""
  hex_str.each_char do |hex_digit|
    val = hex_digit.to_i(16)
    binary_str += val.to_s(2).rjust(4, '0')
  end
  binary_str
end

key_string = File.read("input.txt").strip
total_used = 0

128.times do |i|
  row_key = "#{key_string}-#{i}"
  hash = knot_hash(row_key)
  binary_row = hex_to_binary(hash)
  total_used += binary_row.count('1')
end

puts total_used
