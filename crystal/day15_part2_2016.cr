class Disc
  getter total_positions : Int32
  getter start_position : Int32

  def initialize(total_positions : Int32, start_position : Int32)
    @total_positions = total_positions
    @start_position = start_position
  end
end

discs = [] of Disc
File.open("input.txt") do |file|
  regex = /Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)./
  file.each_line do |line|
    if (matches = regex.match(line))
      total_positions = matches[2].to_i
      start_position = matches[3].to_i
      discs << Disc.new(total_positions, start_position)
    end
  end
end

discs << Disc.new(11, 0)

time = 0
loop do
  if discs.each_with_index.all? { |disc, i| (disc.start_position + time + i + 1) % disc.total_positions == 0 }
    puts time
    break
  end
  time += 1
end