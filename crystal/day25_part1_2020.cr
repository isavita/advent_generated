require "file_utils"
require "big"

def transform(subject_number, loop_size)
  value = 1.to_big_i
  loop_size.times { value = (value * subject_number) % 20201227.to_big_i }
  value
end

def find_loop_size(public_key)
  value = 1.to_big_i
  loop_size = 0
  while value != public_key
    value = (value * 7) % 20201227.to_big_i
    loop_size += 1
  end
  loop_size
end

card_public_key, door_public_key = File.read_lines("input.txt").map(&.to_i64)
card_loop_size = find_loop_size(card_public_key.to_big_i)
encryption_key = transform(door_public_key.to_big_i, card_loop_size)
puts encryption_key