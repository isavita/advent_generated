class Reindeer
  property speed : Int32
  property fly_time : Int32
  property rest_time : Int32
  property distance : Int32 = 0
  property points : Int32 = 0
  property flying : Bool = true
  property time_in_mode : Int32 = 0

  def initialize(@speed : Int32, @fly_time : Int32, @rest_time : Int32)
  end
end

def read_reindeer_details(filename)
  reindeers = [] of Reindeer
  File.each_line(filename) do |line|
    parts = line.split
    speed = parts[3].to_i
    fly_time = parts[6].to_i
    rest_time = parts[13].to_i
    reindeers << Reindeer.new(speed, fly_time, rest_time)
  end
  reindeers
end

def simulate_race_with_points(reindeers, total_seconds)
  total_seconds.times do
    max_distance = 0
    reindeers.each do |reindeer|
      if reindeer.flying
        reindeer.distance += reindeer.speed
      end
      reindeer.time_in_mode += 1
      if (reindeer.flying && reindeer.time_in_mode == reindeer.fly_time) || (!reindeer.flying && reindeer.time_in_mode == reindeer.rest_time)
        reindeer.flying = !reindeer.flying
        reindeer.time_in_mode = 0
      end
      max_distance = reindeer.distance if reindeer.distance > max_distance
    end
    reindeers.each do |reindeer|
      reindeer.points += 1 if reindeer.distance == max_distance
    end
  end
end

def find_max_points(reindeers)
  max_points = 0
  reindeers.each do |reindeer|
    max_points = reindeer.points if reindeer.points > max_points
  end
  max_points
end

reindeers = read_reindeer_details("input.txt")
simulate_race_with_points(reindeers, 2503)
puts find_max_points(reindeers)