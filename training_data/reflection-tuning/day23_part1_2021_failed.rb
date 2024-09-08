class Burrow
  ROOM_POSITIONS = [2, 4, 6, 8]
  HALLWAY_POSITIONS = [0, 1, 3, 5, 7, 9, 10]
  ENERGY_COST = {'A' => 1, 'B' => 10, 'C' => 100, 'D' => 1000}
  ROOM_FOR = {'A' => 0, 'B' => 1, 'C' => 2, 'D' => 3}

  def initialize(rooms, hallway)
    @rooms = rooms
    @hallway = hallway
  end

  def organized?
    @rooms.each_with_index.all? { |room, i| room.all? { |a| ROOM_FOR[a] == i } }
  end

  def possible_moves
    moves = []
    
    # Move from room to hallway
    @rooms.each_with_index do |room, room_index|
      next if room.empty? || room.all? { |a| ROOM_FOR[a] == room_index }
      amphipod = room.first
      room_position = ROOM_POSITIONS[room_index]
      
      HALLWAY_POSITIONS.each do |hallway_position|
        next unless path_clear?(room_position, hallway_position)
        energy = (room.size + (hallway_position - room_position).abs) * ENERGY_COST[amphipod]
        new_rooms = @rooms.map(&:dup)
        new_rooms[room_index].shift
        new_hallway = @hallway.dup
        new_hallway[hallway_position] = amphipod
        moves << [Burrow.new(new_rooms, new_hallway), energy]
      end
    end
    
    # Move from hallway to room
    @hallway.each_with_index do |amphipod, hallway_position|
      next unless amphipod
      room_index = ROOM_FOR[amphipod]
      room_position = ROOM_POSITIONS[room_index]
      next unless path_clear?(hallway_position, room_position)
      next unless @rooms[room_index].all? { |a| ROOM_FOR[a] == room_index }
      
      energy = (@rooms[room_index].size + 1 + (hallway_position - room_position).abs) * ENERGY_COST[amphipod]
      new_rooms = @rooms.map(&:dup)
      new_rooms[room_index].unshift(amphipod)
      new_hallway = @hallway.dup
      new_hallway[hallway_position] = nil
      moves << [Burrow.new(new_rooms, new_hallway), energy]
    end
    
    moves
  end

  def path_clear?(from, to)
    range = from < to ? (from + 1)..to : to..(from - 1)
    range.all? { |pos| @hallway[pos].nil? }
  end

  def hash
    [@rooms, @hallway].hash
  end

  def eql?(other)
    @rooms == other.rooms && @hallway == other.hallway
  end
end

def organize_amphipods(initial_burrow)
  queue = [[initial_burrow, 0]]
  seen = {}

  until queue.empty?
    burrow, energy = queue.shift
    return energy if burrow.organized?

    burrow.possible_moves.each do |new_burrow, move_energy|
      new_energy = energy + move_energy
      if !seen[new_burrow] || seen[new_burrow] > new_energy
        seen[new_burrow] = new_energy
        queue << [new_burrow, new_energy]
      end
    end

    queue.sort_by! { |_, e| e }
  end

  nil
end

# Example usage
initial_rooms = [
  ['B', 'A'],
  ['C', 'D'],
  ['B', 'C'],
  ['D', 'A']
]
initial_hallway = Array.new(11)
initial_burrow = Burrow.new(initial_rooms, initial_hallway)

puts organize_amphipods(initial_burrow)
