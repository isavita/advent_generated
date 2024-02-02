
input = File.readlines('input.txt')

reindeer = {}

input.each do |line|
  data = line.split(' ')
  name = data[0]
  speed = data[3].to_i
  fly_time = data[6].to_i
  rest_time = data[13].to_i
  reindeer[name] = { speed: speed, fly_time: fly_time, rest_time: rest_time, distance: 0, points: 0, flying: true, time_left: fly_time }
end

2503.times do
  reindeer.each do |name, data|
    if data[:flying]
      data[:distance] += data[:speed]
    end
    data[:time_left] -= 1
    if data[:time_left] == 0
      if data[:flying]
        data[:flying] = false
        data[:time_left] = data[:rest_time]
      else
        data[:flying] = true
        data[:time_left] = data[:fly_time]
      end
    end
  end
  max_distance = reindeer.values.map { |r| r[:distance] }.max
  reindeer.each do |name, data|
    if data[:distance] == max_distance
      data[:points] += 1
    end
  end
end

puts reindeer.values.map { |r| r[:points] }.max
