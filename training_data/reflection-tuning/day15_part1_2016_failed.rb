class Disc
  attr_reader :positions, :start_position, :time_offset

  def initialize(positions, start_position, time_offset)
    @positions = positions
    @start_position = start_position
    @time_offset = time_offset
  end

  def position_at(time)
    (start_position + time) % positions
  end
end

def passes?(discs, start_time)
  discs.all? do |disc|
    disc.position_at(start_time + disc.time_offset) == 0
  end
end

discs = [
  Disc.new(5, 4, 1),
  Disc.new(2, 1, 2)
]

start_time = 0
until passes?(discs, start_time)
  start_time += 1
end

puts start_time
