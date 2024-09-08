class Reindeer
  attr_reader :name, :speed, :fly_time, :rest_time
  attr_accessor :distance, :points

  def initialize(name, speed, fly_time, rest_time)
    @name = name
    @speed = speed
    @fly_time = fly_time
    @rest_time = rest_time
    @distance = 0
    @points = 0
  end

  def distance_after(seconds)
    cycle_time = fly_time + rest_time
    full_cycles = seconds / cycle_time
    remaining_seconds = seconds % cycle_time

    distance = full_cycles * speed * fly_time
    distance += [remaining_seconds, fly_time].min * speed
    distance
  end
end

def parse_input(filename)
  File.readlines(filename).map do |line|
    name, speed, fly_time, rest_time = line.match(/(\w+) can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds./).captures
    Reindeer.new(name, speed.to_i, fly_time.to_i, rest_time.to_i)
  end
end

def simulate_race(reindeers, total_seconds)
  (1..total_seconds).each do |second|
    reindeers.each do |reindeer|
      reindeer.distance = reindeer.distance_after(second)
    end
    
    max_distance = reindeers.map(&:distance).max
    reindeers.select { |r| r.distance == max_distance }.each { |r| r.points += 1 }
  end
end

reindeers = parse_input('input.txt')
total_seconds = 2503

# Part 1
max_distance = reindeers.map { |r| r.distance_after(total_seconds) }.max
puts "Part 1: The winning reindeer traveled #{max_distance} km"

# Part 2
simulate_race(reindeers, total_seconds)
max_points = reindeers.map(&:points).max
puts "Part 2: The winning reindeer has #{max_points} points"
