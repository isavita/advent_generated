
players, last_marble = File.read("input.txt").scan(/\d+/).map(&:to_i)

circle = [0]
current_index = 0
scores = Hash.new(0)

(1..last_marble).each do |marble|
  if marble % 23 == 0
    current_index = (current_index - 7) % circle.length
    scores[marble % players] += marble + circle.delete_at(current_index)
  else
    current_index = (current_index + 2) % circle.length
    circle.insert(current_index, marble)
  end
end

puts scores.values.max
