
def transform_subject_number(subject_number, loop_size)
  value = 1
  loop_size.times do
    value = (value * subject_number) % 20201227
  end
  value
end

card_public_key, door_public_key = File.read('input.txt').split("\n").map(&:to_i)

card_loop_size = 0
value = 1
while value != card_public_key
  value = (value * 7) % 20201227
  card_loop_size += 1
end

encryption_key = transform_subject_number(door_public_key, card_loop_size)
puts encryption_key
