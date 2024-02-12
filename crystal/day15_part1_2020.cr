
input = File.read("input.txt").chomp.split(",").map(&.to_i)

last_spoken = Hash(Int32, Int32).new
last_number = 0
next_number = 0

(1..2020).each do |turn|
  if turn - 1 < input.size
    last_number = input[turn - 1]
    last_spoken[last_number] = turn
    next
  end

  if last_turn = last_spoken[last_number]?
    next_number = turn - 1 - last_turn
  else
    next_number = 0
  end

  last_spoken[last_number] = turn - 1
  last_number = next_number
end

puts last_number
