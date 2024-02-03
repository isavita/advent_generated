
fish = File.read('input.txt').split(',').map(&:to_i)

80.times do
  new_fish = []
  fish.each_with_index do |f, i|
    if f == 0
      fish[i] = 6
      new_fish << 8
    else
      fish[i] -= 1
    end
  end
  fish += new_fish
end

puts fish.size
