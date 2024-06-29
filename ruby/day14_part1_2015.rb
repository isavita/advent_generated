
Reindeer = Struct.new(:speed, :fly_time, :rest_time, :distance, :flying, :time_in_mode)

def read_reindeer_details(filename)
  File.readlines(filename).map do |line|
    parts = line.split
    Reindeer.new(parts[3].to_i, parts[6].to_i, parts[13].to_i, 0, true, 0)
  end
end

def simulate_race(reindeers, total_seconds)
  total_seconds.times do
    reindeers.each do |r|
      if r.flying
        r.distance += r.speed
        r.time_in_mode += 1
        if r.time_in_mode == r.fly_time
          r.flying = false
          r.time_in_mode = 0
        end
      else
        r.time_in_mode += 1
        if r.time_in_mode == r.rest_time
          r.flying = true
          r.time_in_mode = 0
        end
      end
    end
  end
end

reindeers = read_reindeer_details("input.txt")
simulate_race(reindeers, 2503)
puts reindeers.map(&:distance).max
