ingredients = File.readlines('input.txt').map do |line|
  line.scan(/-?\d+/).map(&:to_i)
end

max_score = 0

(0..100).each do |i|
  (0..100-i).each do |j|
    (0..100-i-j).each do |k|
      l = 100 - i - j - k
      capacity = [0, i * ingredients[0][0] + j * ingredients[1][0] + k * ingredients[2][0] + l * ingredients[3][0]].max
      durability = [0, i * ingredients[0][1] + j * ingredients[1][1] + k * ingredients[2][1] + l * ingredients[3][1]].max
      flavor = [0, i * ingredients[0][2] + j * ingredients[1][2] + k * ingredients[2][2] + l * ingredients[3][2]].max
      texture = [0, i * ingredients[0][3] + j * ingredients[1][3] + k * ingredients[2][3] + l * ingredients[3][3]].max
      calories = i * ingredients[0][4] + j * ingredients[1][4] + k * ingredients[2][4] + l * ingredients[3][4]

      score = capacity * durability * flavor * texture
      max_score = [max_score, score].max if calories == 500
    end
  end
end

puts max_score