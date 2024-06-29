
Reindeer = Struct.new(:speed, :fly_time, :rest_time, :distance, :points, :flying, :time_in_mode)

def read_reindeer_details(filename)
  File.readlines(filename).map do |line|
    parts = line.split
    Reindeer.new(parts[3].to_i, parts[6].to_i, parts[13].to_i, 0, 0, true, 0)
  end
end

def simulate_race_with_points(reindeers, total_seconds)
  total_seconds.times do
    max_distance = reindeers.map do |r|
      r.distance += r.speed if r.flying
      r.time_in_mode += 1
      if (r.flying && r.time_in_mode == r.fly_time) || (!r.flying && r.time_in_mode == r.rest_time)
        r.flying = !r.flying
        r.time_in_mode = 0
      end
      r.distance
    end.max

    reindeers.each { |r| r.points += 1 if r.distance == max_distance }
  end
end

reindeers = read_reindeer_details('input.txt')
simulate_race_with_points(reindeers, 2503)
puts reindeers.map(&:points).max
