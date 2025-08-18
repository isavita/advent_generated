
# frozen_string_literal: true

players, _, _, _, _, _, last_mul = File.read("input.txt").strip.split
players = players.to_i
last_marble = last_mul.to_i * 100

next_idx = Array(Int32).new(last_marble + 1) { |i| i }
prev_idx = Array(Int32).new(last_marble + 1) { |i| i }

scores = Array(Int64).new(players, 0_i64)
current = 0

(1..last_marble).each do |marble|
  player = (marble - 1) % players
  if marble % 23 == 0
    7.times { current = prev_idx[current] }
    scores[player] += marble.to_i64 + current.to_i64
    left = prev_idx[current]
    right = next_idx[current]
    next_idx[left] = right
    prev_idx[right] = left
    current = right
  else
    current = next_idx[current]
    right = next_idx[current]
    next_idx[current] = marble
    prev_idx[marble] = current
    next_idx[marble] = right
    prev_idx[right] = marble
    current = marble
  end
end

puts scores.max
