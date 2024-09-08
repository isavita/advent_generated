class Burrow
  ROOM_POSITIONS = [2, 4, 6, 8]
  ENERGY = { 'A' => 1, 'B' => 10, 'C' => 100, 'D' => 1000 }
  ROOM_FOR = { 'A' => 0, 'B' => 1, 'C' => 2, 'D' => 3 }

  def initialize(rooms)
    @rooms = rooms
    @hallway = Array.new(11, '.')
  end

  def solved?
    @rooms.each_with_index.all? { |room, i| room.all? { |a| ROOM_FOR[a] == i } }
  end

  def next_states
    states = []

    # Move from hallway to room
    @hallway.each_with_index do |amphipod, i|
      next if amphipod == '.'
      room_index = ROOM_FOR[amphipod]
      room = @rooms[room_index]
      if can_move_to_room?(amphipod, room) && path_clear?(i, ROOM_POSITIONS[room_index])
        new_hallway = @hallway.dup
        new_hallway[i] = '.'
        new_rooms = @rooms.map(&:dup)
        new_rooms[room_index] = [amphipod] + room
        energy = (ROOM_POSITIONS[room_index] - i).abs + (4 - room.size)
        states << [Burrow.new(new_rooms), energy * ENERGY[amphipod]]
      end
    end

    # Move from room to hallway
    @rooms.each_with_index do |room, room_index|
      next if room.empty? || room.all? { |a| ROOM_FOR[a] == room_index }
      amphipod = room.first
      (0...11).each do |hallway_pos|
        next if ROOM_POSITIONS.include?(hallway_pos)
        if path_clear?(ROOM_POSITIONS[room_index], hallway_pos)
          new_hallway = @hallway.dup
          new_hallway[hallway_pos] = amphipod
          new_rooms = @rooms.map(&:dup)
          new_rooms[room_index] = room[1..-1]
          energy = (ROOM_POSITIONS[room_index] - hallway_pos).abs + (4 - room.size + 1)
          states << [Burrow.new(new_rooms), energy * ENERGY[amphipod]]
        end
      end
    end

    states
  end

  def can_move_to_room?(amphipod, room)
    room.empty? || (room.all? { |a| a == amphipod } && room.size < 4)
  end

  def path_clear?(from, to)
    range = from < to ? (from + 1)..to : to..(from - 1)
    range.all? { |i| @hallway[i] == '.' }
  end

  def to_s
    @rooms.map(&:join).join(',') + '|' + @hallway.join
  end
end

def solve(initial_burrow)
  queue = [[initial_burrow, 0]]
  seen = {}

  while !queue.empty?
    burrow, energy = queue.shift
    next if seen[burrow.to_s] && seen[burrow.to_s] <= energy
    seen[burrow.to_s] = energy

    return energy if burrow.solved?

    burrow.next_states.each do |next_burrow, additional_energy|
      queue << [next_burrow, energy + additional_energy]
    end
    queue.sort_by! { |_, e| e }
  end
end

# Parse input
input = File.readlines('input.txt', chomp: true)
rooms = [[], [], [], []]
input[2..3].each do |line|
  line.scan(/[ABCD]/).each_with_index { |a, i| rooms[i].unshift(a) }
end

# Part 1
puts solve(Burrow.new(rooms))

# Part 2
rooms.each_with_index do |room, i|
  room.insert(1, ['D', 'C', 'B', 'A'][i])
  room.insert(1, ['D', 'B', 'A', 'C'][i])
end

puts solve(Burrow.new(rooms))
