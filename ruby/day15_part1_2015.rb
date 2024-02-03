
ingredients = []
File.open("input.txt").each do |line|
  parts = line.split(" ")
  ingredients << {
    capacity: parts[2].to_i,
    durability: parts[4].to_i,
    flavor: parts[6].to_i,
    texture: parts[8].to_i,
    calories: parts[10].to_i
  }
end

max_score = 0
(0..100).each do |i|
  (0..100-i).each do |j|
    (0..100-i-j).each do |k|
      l = 100 - i - j - k
      capacity = [0, i*ingredients[0][:capacity] + j*ingredients[1][:capacity] + k*ingredients[2][:capacity] + l*ingredients[3][:capacity]].max
      durability = [0, i*ingredients[0][:durability] + j*ingredients[1][:durability] + k*ingredients[2][:durability] + l*ingredients[3][:durability]].max
      flavor = [0, i*ingredients[0][:flavor] + j*ingredients[1][:flavor] + k*ingredients[2][:flavor] + l*ingredients[3][:flavor]].max
      texture = [0, i*ingredients[0][:texture] + j*ingredients[1][:texture] + k*ingredients[2][:texture] + l*ingredients[3][:texture]].max
      score = capacity * durability * flavor * texture
      max_score = [max_score, score].max
    end
  end
end

puts max_score
