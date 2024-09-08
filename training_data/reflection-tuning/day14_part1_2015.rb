class Reindeer
  attr_reader :name, :speed, :fly_time, :rest_time

  def initialize(name, speed, fly_time, rest_time)
    @name = name
    @speed = speed
    @fly_time = fly_time
    @rest_time = rest_time
  end

  def distance_traveled(total_time)
    cycle_time = fly_time + rest_time
    full_cycles, remaining_time = total_time.divmod(cycle_time)
    
    distance = full_cycles * speed * fly_time
    
    if remaining_time > fly_time
      distance += speed * fly_time
    else
      distance += speed * remaining_time
    end
    
    distance
  end
end

reindeer = File.readlines('input.txt').map do |line|
  name, _, _, speed, _, _, fly_time, _, _, _, _, _, _, rest_time, _ = line.split
  Reindeer.new(name, speed.to_i, fly_time.to_i, rest_time.to_i)
end

race_time = 2503
max_distance = reindeer.map { |r| r.distance_traveled(race_time) }.max

puts max_distance
