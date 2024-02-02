
input = File.read('input.txt').split(',').map(&:to_i)

last_spoken = {}
input.each_with_index { |num, idx| last_spoken[num] = idx + 1 }

turn = input.length + 1
prev_num = input.last

while turn <= 2020
  if last_spoken[prev_num].nil?
    spoken = 0
  else
    spoken = turn - 1 - last_spoken[prev_num]
  end

  last_spoken[prev_num] = turn - 1
  prev_num = spoken
  turn += 1
end

puts prev_num
