# Intcode Computer Implementation (Day 9 & 17 Compliant)
class Intcode
  property memory : Hash(Int64, Int64)
  property ip : Int64
  property relative_base : Int64
  property inputs : Deque(Int64)
  property halted : Bool
  property waiting_for_input : Bool

  def initialize(program : Array(Int64))
    @memory = Hash(Int64, Int64).new(0_i64)
    program.each_with_index { |v, i| @memory[i.to_i64] = v }
    @ip = 0_i64
    @relative_base = 0_i64
    @inputs = Deque(Int64).new
    @halted = false
    @waiting_for_input = false
  end

  def input_ascii(str : String)
    str.each_char { |c| @inputs << c.ord.to_i64 }
    @inputs << 10_i64 # Newline
  end

  # Run until output produced, waiting for input, or halted
  def run : Int64?
    loop do
      return nil if @halted
      
      opcode_full = @memory[@ip]
      opcode = opcode_full % 100
      modes = (opcode_full // 100).to_s.reverse.chars.map { |c| c.to_i }

      get_param = ->(offset : Int32) {
        mode = modes[offset - 1]? || 0
        val = @memory[@ip + offset]
        case mode
        when 0 then @memory[val]
        when 1 then val
        when 2 then @memory[@relative_base + val]
        else 0_i64
        end
      }

      get_addr = ->(offset : Int32) {
        mode = modes[offset - 1]? || 0
        val = @memory[@ip + offset]
        case mode
        when 0 then val
        when 2 then @relative_base + val
        else 0_i64
        end
      }

      case opcode
      when 1 # Add
        p1, p2 = get_param.call(1), get_param.call(2)
        dest = get_addr.call(3)
        @memory[dest] = p1 + p2
        @ip += 4
      when 2 # Multiply
        p1, p2 = get_param.call(1), get_param.call(2)
        dest = get_addr.call(3)
        @memory[dest] = p1 * p2
        @ip += 4
      when 3 # Input
        if @inputs.empty?
          @waiting_for_input = true
          return nil 
        end
        @waiting_for_input = false
        dest = get_addr.call(1)
        @memory[dest] = @inputs.shift
        @ip += 2
      when 4 # Output
        val = get_param.call(1)
        @ip += 2
        return val
      when 5 # Jump-if-true
        p1, p2 = get_param.call(1), get_param.call(2)
        @ip = (p1 != 0 ? p2 : @ip + 3)
      when 6 # Jump-if-false
        p1, p2 = get_param.call(1), get_param.call(2)
        @ip = (p1 == 0 ? p2 : @ip + 3)
      when 7 # Less than
        p1, p2 = get_param.call(1), get_param.call(2)
        dest = get_addr.call(3)
        @memory[dest] = (p1 < p2 ? 1_i64 : 0_i64)
        @ip += 4
      when 8 # Equals
        p1, p2 = get_param.call(1), get_param.call(2)
        dest = get_addr.call(3)
        @memory[dest] = (p1 == p2 ? 1_i64 : 0_i64)
        @ip += 4
      when 9 # Adjust relative base
        p1 = get_param.call(1)
        @relative_base += p1
        @ip += 2
      when 99 # Halt
        @halted = true
        return nil
      else
        raise "Unknown opcode: #{opcode}"
      end
    end
  end

  def read_ascii_output : String
    io = IO::Memory.new
    loop do
      res = run
      break if res.nil?
      io << res.to_u8.chr
    end
    io.to_s
  end
end

class DroidSolver
  # Items that cause game over or stuck state
  BAD_ITEMS = Set.new([
    "infinite loop", "giant electromagnet", "photons", 
    "molten lava", "escape pod"
  ])

  OPPOSITE = {"north" => "south", "south" => "north", "east" => "west", "west" => "east"}

  def initialize
    input_text = File.read("input.txt").strip
    program = input_text.split(',').map(&.to_i64)
    @computer = Intcode.new(program)
    
    # Graph: RoomName -> { direction -> NeighborRoomName }
    @map = Hash(String, Hash(String, String)).new { |h, k| h[k] = Hash(String, String).new }
    @visited_rooms = Set(String).new
    @security_checkpoint = ""
    @sensor_direction = ""
    @inventory = [] of String
  end

  def solve
    puts "Booting Droid..."
    output = @computer.read_ascii_output
    
    # Start recursive DFS exploration
    explore_dfs(output)

    if @security_checkpoint.empty? || @sensor_direction.empty?
      puts "Failed to find Security Checkpoint or Sensor!"
      return
    end

    puts "\n--- Exploration Complete ---"
    puts "Checkpoint: #{@security_checkpoint}"
    puts "Sensor Direction: #{@sensor_direction}"
    puts "Inventory: #{@inventory.join(", ")}"

    # Navigate to Checkpoint
    # We find path from current location (assumed Start/Hull Breach after DFS stack unwinds)
    # But just in case, we assume we are at the room printed last.
    
    # Since recursion returns us to start, let's pathfind from "Hull Breach" to Checkpoint.
    # Note: If your input start name is different, this might need adjustment, 
    # but usually it's "Hull Breach".
    start_room = @visited_rooms.find { |r| r == "Hull Breach" } || @visited_rooms.first
    
    if start_room
        path = bfs_path(start_room, @security_checkpoint)
        puts "Navigating to checkpoint..."
        path.each do |dir|
            execute_command(dir)
        end
    else
        puts "Error: Could not determine start room."
    end

    # Now at checkpoint. Begin brute force.
    brute_force_sensor
  end

  def execute_command(cmd : String) : String
    @computer.input_ascii(cmd)
    @computer.read_ascii_output
  end

  # Recursive DFS to map world and collect items
  def explore_dfs(initial_output : String)
    current_desc = initial_output
    room_name = parse_room_name(current_desc)
    return if room_name.nil?

    @visited_rooms.add(room_name)

    # 1. Collect Items
    items = parse_items(current_desc)
    items.each do |item|
      unless BAD_ITEMS.includes?(item)
        puts "Taking #{item} in #{room_name}"
        execute_command("take #{item}")
        @inventory << item
      end
    end

    # 2. Identify Doors
    doors = parse_doors(current_desc)

    # 3. Try each door
    doors.each do |dir|
      if @map[room_name].has_key?(dir)
        next
      end

      move_out = execute_command(dir)
      new_room_name = parse_room_name(move_out)

      # Check if this move triggered the sensor (ejected back)
      # Usually identified by "Alert" or "heavier/lighter" in output
      if move_out.includes?("Alert!") || move_out.includes?("heavier") || move_out.includes?("lighter")
        puts "Found Sensor in direction: #{dir}"
        @security_checkpoint = room_name
        @sensor_direction = dir
        @map[room_name][dir] = "SENSOR"
        # Since we were ejected, we are still in room_name, no need to move back.
      
      elsif new_room_name.nil?
        # Wall or locked door that didn't print room name
        # Do nothing (effectively a dead end)
      
      elsif new_room_name == room_name
        # Moved but stayed in same room? (Shouldn't happen on standard doors unless sensor)
        # Treat as no-op.
      
      else
        # Success - entered new room
        # We perform the assignment only if new_room_name is strictly a String
        if new_room_name
            @map[room_name][dir] = new_room_name
            @map[new_room_name][OPPOSITE[dir]] = room_name
            
            # Recurse
            explore_dfs(move_out)
            
            # Backtrack
            execute_command(OPPOSITE[dir])
        end
      end
    end
  end

  def bfs_path(start : String, target : String) : Array(String)
    q = Deque({String, Array(String)}).new
    q << {start, [] of String}
    visited = Set(String).new
    visited.add(start)

    while !q.empty?
      curr, path = q.shift
      return path if curr == target

      @map[curr].each do |dir, neighbor|
        next if neighbor == "SENSOR"
        unless visited.includes?(neighbor)
          visited.add(neighbor)
          new_path = path.dup
          new_path << dir
          q << {neighbor, new_path}
        end
      end
    end
    [] of String
  end

  def brute_force_sensor
    puts "Starting Brute Force on #{@inventory.size} items..."
    
    # Drop everything first
    @inventory.each do |item|
      execute_command("drop #{item}")
    end

    current_held = Set(String).new
    
    # Iterate using Gray Code to minimize moves
    (0...(1 << @inventory.size)).each do |i|
      gray = i ^ (i >> 1)
      
      target_held = Set(String).new
      @inventory.each_with_index do |item, idx|
        if ((gray >> idx) & 1) == 1
          target_held.add(item)
        end
      end
      
      # Adjust inventory
      to_drop = current_held - target_held
      to_take = target_held - current_held
      
      to_drop.each do |item| 
        execute_command("drop #{item}")
        current_held.delete(item)
      end
      
      to_take.each do |item| 
        execute_command("take #{item}")
        current_held.add(item)
      end
      
      # Try sensor
      res = execute_command(@sensor_direction)
      
      # If we don't see "Security Checkpoint" (meaning we weren't ejected back)
      # OR we see "Analysis complete", we likely passed.
      if !res.includes?("Security Checkpoint") || res.includes?("Analysis complete")
        puts "\n\nSUCCESS!"
        puts res
        
        if match = res.match(/(\d+)/)
           puts "Password found: #{match[1]}"
        end
        return
      end
    end
  end

  def parse_room_name(text : String) : String?
    if match = text.match(/== (.+) ==/)
      match[1]
    else
      if text.includes?("Pressure-Sensitive Floor")
         "Pressure-Sensitive Floor"
      else
         nil
      end
    end
  end

  def parse_doors(text : String) : Array(String)
    doors = [] of String
    reading_doors = false
    text.each_line do |line|
      if line.includes?("Doors here lead:")
        reading_doors = true
        next
      end
      if reading_doors
        if line.starts_with?("- ")
          doors << line[2..-1].strip
        elsif line.strip.empty?
          reading_doors = false
        end
      end
    end
    doors
  end

  def parse_items(text : String) : Array(String)
    items = [] of String
    reading_items = false
    text.each_line do |line|
      if line.includes?("Items here:")
        reading_items = true
        next
      end
      if reading_items
        if line.starts_with?("- ")
          items << line[2..-1].strip
        elsif line.strip.empty?
          reading_items = false
        end
      end
    end
    items
  end
end

solver = DroidSolver.new
solver.solve
