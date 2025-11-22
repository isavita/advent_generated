# Priority Queue implementation for A*
class MinHeap(T)
  def initialize
    @elements = [] of T
    @block = ->(a : T, b : T) { a <=> b }
  end

  def initialize(&block : (T, T) -> Int32)
    @elements = [] of T
    @block = block
  end

  def empty?
    @elements.empty?
  end

  def push(element : T)
    @elements << element
    sift_up(@elements.size - 1)
  end

  def pop : T
    return @elements.pop if @elements.size <= 1
    top = @elements[0]
    @elements[0] = @elements.pop
    sift_down(0)
    top
  end

  private def sift_up(index)
    while index > 0
      parent_index = (index - 1) // 2
      if @block.call(@elements[index], @elements[parent_index]) < 0
        @elements[index], @elements[parent_index] = @elements[parent_index], @elements[index]
        index = parent_index
      else
        break
      end
    end
  end

  private def sift_down(index)
    loop do
      child_index = 2 * index + 1
      break if child_index >= @elements.size

      if child_index + 1 < @elements.size &&
         @block.call(@elements[child_index + 1], @elements[child_index]) < 0
        child_index += 1
      end

      if @block.call(@elements[child_index], @elements[index]) < 0
        @elements[index], @elements[child_index] = @elements[child_index], @elements[index]
        index = child_index
      else
        break
      end
    end
  end
end

# Constants
ROOM_HALLWAY_INDICES = {0 => 2, 1 => 4, 2 => 6, 3 => 8}
COSTS = {0 => 1, 1 => 10, 2 => 100, 3 => 1000} # 0=A, 1=B, 2=C, 3=D
HALLWAY_LEN = 11
ROOM_SIZE = 4

# Represent the layout as an immutable structure for Hash keys
alias Hallway = Tuple(Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
alias Room = Tuple(Int8, Int8, Int8, Int8)
alias Rooms = Tuple(Room, Room, Room, Room)

struct State
  property hallway : Hallway
  property rooms : Rooms
  property cost : Int32
  property heuristic : Int32

  def initialize(@hallway, @rooms, @cost = 0)
    @heuristic = calculate_heuristic
  end

  # Calculate minimum energy to move everyone home ignoring blocking
  def calculate_heuristic : Int32
    h = 0
    
    # Hallway costs
    @hallway.each_with_index do |pod, idx|
      next if pod == -1 # Empty
      
      target_room_idx = pod
      target_hallway_idx = ROOM_HALLWAY_INDICES[target_room_idx]
      
      dist = (idx - target_hallway_idx).abs
      # +1 for entering the room (minimum)
      h += (dist + 1) * COSTS[pod]
    end

    # Room costs
    @rooms.each_with_index do |room, r_idx|
      room.each_with_index do |pod, depth|
        next if pod == -1 # Empty (shouldn't happen in valid states usually)
        
        target_room_idx = pod
        
        # Check if already in correct place
        is_correct = (r_idx == target_room_idx)
        
        # Even if in correct room, if there's a wrong pod below, it must move
        if is_correct
          below_ok = true
          ((depth + 1)...ROOM_SIZE).each do |d2|
            if room[d2] != target_room_idx
              below_ok = false
              break
            end
          end
          
          # It's in the right place and everything below is correct
          next if below_ok
        end

        # Needs to move out + move to target + move in
        # Move out to hallway entrance
        steps_out = depth + 1
        
        # Move along hallway
        current_hallway_idx = ROOM_HALLWAY_INDICES[r_idx]
        target_hallway_idx = ROOM_HALLWAY_INDICES[target_room_idx]
        steps_hallway = (current_hallway_idx - target_hallway_idx).abs
        
        # Move in (min 1 step)
        steps_in = 1
        
        total_steps = steps_out + steps_hallway + steps_in
        h += total_steps * COSTS[pod]
      end
    end
    h
  end

  # Equality and Hashing for visited set
  def_equals_and_hash hallway, rooms
  
  def total_score
    @cost + @heuristic
  end
end

def solve
  lines = File.read_lines("input.txt")

  # Extract initial rows
  # Input format usually:
  # #############
  # #...........#
  # ###B#C#B#D###  <- Line 2
  #   #A#D#C#A#    <- Line 3
  #   #########
  
  row1 = [lines[2][3], lines[2][5], lines[2][7], lines[2][9]].map { |c| parse_char(c) }
  row4 = [lines[3][3], lines[3][5], lines[3][7], lines[3][9]].map { |c| parse_char(c) }

  # Part 2 Inserted Rows
  # #D#C#B#A#
  # #D#B#A#C#
  row2 = [3, 2, 1, 0].map(&.to_i8) # D C B A
  row3 = [3, 1, 0, 2].map(&.to_i8) # D B A C

  initial_rooms_arr = [] of Room
  4.times do |i|
    initial_rooms_arr << {row1[i], row2[i], row3[i], row4[i]}
  end

  initial_hallway = Tuple(Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8).from(Array.new(11, -1_i8))
  initial_rooms = Rooms.from(initial_rooms_arr)

  start_state = State.new(initial_hallway, initial_rooms, 0)

  # MinHeap stores States, ordered by total_score (A*)
  pq = MinHeap(State).new { |a, b| a.total_score <=> b.total_score }
  pq.push(start_state)

  # Visited stores min cost to reach a specific configuration (hallway + rooms)
  visited = Hash({Hallway, Rooms}, Int32).new

  # Helper to get path range
  path_range = ->(a : Int32, b : Int32) {
    a < b ? (a + 1)..b : b..(a - 1)
  }

  while !pq.empty?
    current = pq.pop

    # Check if solved
    if solved?(current.rooms)
      puts "Part 2 Solution: #{current.cost}"
      return
    end

    # If we found a cheaper way to this state already, skip
    state_key = {current.hallway, current.rooms}
    if visited.has_key?(state_key) && visited[state_key] <= current.cost
      next
    end
    visited[state_key] = current.cost

    # --- Move Logic ---

    # optimization: if a pod can go home, do that and ONLY that.
    moved_home = false
    
    # 1. Check Hallway -> Room
    current.hallway.each_with_index do |pod, h_idx|
      next if pod == -1
      
      target_room = pod.to_i
      target_h_idx = ROOM_HALLWAY_INDICES[target_room]
      
      # Check path clear
      path_blocked = false
      path_range.call(h_idx, target_h_idx).each do |k|
        if current.hallway[k] != -1
          path_blocked = true
          break
        end
      end
      next if path_blocked

      # Check room valid (contains only same type or empty)
      room_content = current.rooms[target_room]
      can_enter = true
      target_depth = -1
      
      (ROOM_SIZE - 1).downto(0) do |d|
        if room_content[d] == -1
          target_depth = d
          break
        elsif room_content[d] != target_room
          can_enter = false
          break
        end
      end
      
      if can_enter && target_depth != -1
        # Move Hallway -> Room
        cost_mult = COSTS[target_room]
        steps = (h_idx - target_h_idx).abs + (target_depth + 1)
        new_cost = current.cost + steps * cost_mult
        
        new_hallway = current.hallway.dup
        # Tuple is immutable, need to convert to array to modify then back to tuple? 
        # In Crystal, we can't modify Tuples. We construct new ones.
        # For speed, let's use map or specific replacement.
        
        new_hallway_arr = new_hallway.to_a
        new_hallway_arr[h_idx] = -1_i8
        
        new_rooms_arr = current.rooms.to_a
        room_arr = new_rooms_arr[target_room].to_a
        room_arr[target_depth] = pod
        new_rooms_arr[target_room] = Room.from(room_arr)
        
        next_state = State.new(
          Hallway.from(new_hallway_arr), 
          Rooms.from(new_rooms_arr), 
          new_cost
        )
        
        pq.push(next_state)
        moved_home = true # optimization found
      end
    end

    next if moved_home # If we moved someone home, skip generating other moves

    # 2. Check Room -> Hallway
    current.rooms.each_with_index do |room, r_idx|
      # Check if room needs to move
      # It needs to move if there is ANY pod strictly inside that doesn't belong
      needs_to_move = false
      room.each do |p|
        if p != -1 && p != r_idx
          needs_to_move = true
          break
        end
      end
      
      # Also needs to move if it's the correct type but blocking a wrong type below
      # Actually the loop above covers "blocking a wrong type" because if there is a wrong type,
      # anyone above it must eventually move.
      # However, we must pick the TOPMOST pod.
      
      next unless needs_to_move

      # Find top pod
      src_depth = -1
      pod_type = -1_i8
      room.each_with_index do |p, d|
        if p != -1
          src_depth = d
          pod_type = p
          break
        end
      end
      
      next if src_depth == -1 # Empty room

      # Check if this specific pod needs to move?
      # If it is the correct type, check if everyone below is also correct.
      if pod_type == r_idx
        all_below_correct = true
        ((src_depth + 1)...ROOM_SIZE).each do |d|
          if room[d] != r_idx
            all_below_correct = false
            break
          end
        end
        next if all_below_correct
      end

      # Try moving to all reachable hallway spots
      room_h_idx = ROOM_HALLWAY_INDICES[r_idx]
      
      [0, 1, 3, 5, 7, 9, 10].each do |h_target|
        # Check path
        blocked = false
        path_range.call(room_h_idx, h_target).each do |k|
          if current.hallway[k] != -1
            blocked = true
            break
          end
        end
        next if blocked

        # Create State
        cost_mult = COSTS[pod_type]
        steps = (src_depth + 1) + (room_h_idx - h_target).abs
        new_cost = current.cost + steps * cost_mult

        new_rooms_arr = current.rooms.to_a
        src_room_arr = new_rooms_arr[r_idx].to_a
        src_room_arr[src_depth] = -1_i8
        new_rooms_arr[r_idx] = Room.from(src_room_arr)
        
        new_hallway_arr = current.hallway.to_a
        new_hallway_arr[h_target] = pod_type
        
        next_state = State.new(
          Hallway.from(new_hallway_arr),
          Rooms.from(new_rooms_arr),
          new_cost
        )
        
        pq.push(next_state)
      end
    end
  end
end

def parse_char(c : Char) : Int8
  case c
  when 'A' then 0_i8
  when 'B' then 1_i8
  when 'C' then 2_i8
  when 'D' then 3_i8
  else -1_i8
  end
end

def solved?(rooms : Rooms)
  rooms.each_with_index do |room, r_idx|
    room.each do |pod|
      return false if pod != r_idx
    end
  end
  true
end

solve
