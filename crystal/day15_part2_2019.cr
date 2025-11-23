# Intcode Computer Implementation for AoC 2019
class Intcode
  property memory : Hash(Int64, Int64)
  property ip : Int64
  property relative_base : Int64
  property inputs : Array(Int64)
  property halted : Bool

  def initialize(program : Array(Int64))
    @memory = Hash(Int64, Int64).new(0_i64)
    program.each_with_index { |v, i| @memory[i.to_i64] = v }
    @ip = 0_i64
    @relative_base = 0_i64
    @inputs = [] of Int64
    @halted = false
  end

  # Enqueues input
  def input(val : Int64 | Int32)
    @inputs << val.to_i64
  end

  # Runs until an output is produced or the program halts
  def run_until_output : Int64?
    loop do
      return nil if @halted
      
      opcode_full = @memory[@ip]
      opcode = opcode_full % 100
      modes = (opcode_full // 100).to_s.reverse.chars.map { |c| c.to_i }

      # Helper to get parameter value based on mode
      get_param = ->(offset : Int32) {
        mode = modes[offset - 1]? || 0
        val = @memory[@ip + offset]
        case mode
        when 0 then @memory[val]     # Position
        when 1 then val              # Immediate
        when 2 then @memory[@relative_base + val] # Relative
        else 0_i64
        end
      }

      # Helper to get write address based on mode
      get_addr = ->(offset : Int32) {
        mode = modes[offset - 1]? || 0
        val = @memory[@ip + offset]
        case mode
        when 0 then val
        when 2 then @relative_base + val
        else 0_i64 # Should not happen for writes
        end
      }

      case opcode
      when 1 # Add
        p1 = get_param.call(1)
        p2 = get_param.call(2)
        dest = get_addr.call(3)
        @memory[dest] = p1 + p2
        @ip += 4
      when 2 # Multiply
        p1 = get_param.call(1)
        p2 = get_param.call(2)
        dest = get_addr.call(3)
        @memory[dest] = p1 * p2
        @ip += 4
      when 3 # Input
        if @inputs.empty?
          # Wait for input (caller must supply input and call again)
          # For this specific problem, we expect input to be ready when asked
          raise "Intcode Error: Waiting for input but buffer empty"
        end
        dest = get_addr.call(1)
        @memory[dest] = @inputs.shift
        @ip += 2
      when 4 # Output
        val = get_param.call(1)
        @ip += 2
        return val
      when 5 # Jump-if-true
        p1 = get_param.call(1)
        p2 = get_param.call(2)
        if p1 != 0
          @ip = p2
        else
          @ip += 3
        end
      when 6 # Jump-if-false
        p1 = get_param.call(1)
        p2 = get_param.call(2)
        if p1 == 0
          @ip = p2
        else
          @ip += 3
        end
      when 7 # Less than
        p1 = get_param.call(1)
        p2 = get_param.call(2)
        dest = get_addr.call(3)
        @memory[dest] = (p1 < p2 ? 1_i64 : 0_i64)
        @ip += 4
      when 8 # Equals
        p1 = get_param.call(1)
        p2 = get_param.call(2)
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
        raise "Unknown opcode: #{opcode} at IP #{@ip}"
      end
    end
  end
end

# --- Droid Logic ---

# Directions: 1=North, 2=South, 3=West, 4=East
DIRS = {
  1 => {0, 1},
  2 => {0, -1},
  3 => {-1, 0},
  4 => {1, 0}
}

REVERSE_DIRS = {
  1 => 2,
  2 => 1,
  3 => 4,
  4 => 3
}

# Grid state: 0=Wall, 1=Empty, 2=Oxygen
alias Point = Tuple(Int32, Int32)
grid = Hash(Point, Int32).new
visited = Set(Point).new
oxygen_location = {0, 0}

# Read Input
input_text = File.read("input.txt").strip
program = input_text.split(',').map(&.to_i64)

computer = Intcode.new(program)

# DFS Traversal to map the area
# We start at 0,0. The computer is also at 0,0.
# This recursive function moves the robot, explores, and moves it back.
def explore(
  pos : Point, 
  computer : Intcode, 
  grid : Hash(Point, Int32), 
  visited : Set(Point)
) : Point? # Returns oxygen location if found locally, else nil logic handles externally
  
  x, y = pos
  visited.add(pos)
  found_oxy = nil

  # Try all 4 directions
  DIRS.each do |cmd, (dx, dy)|
    nx, ny = x + dx, y + dy
    next_pos = {nx, ny}

    # If we haven't visited this neighbor, try to move there
    unless visited.includes?(next_pos)
      # Send move command
      computer.input(cmd)
      status = computer.run_until_output

      if status.nil?
        raise "Computer halted unexpectedly"
      end

      # Record map data
      grid[next_pos] = status.to_i

      if status == 0
        # Hit a wall, robot position didn't change.
        # Just mark visited (implied by grid presence if we wanted, but visited set works)
        # Note: 'visited' set usually tracks where we stood. 
        # We can add walls to visited so we don't bump them again, 
        # or just rely on grid check. Here we rely on recursive structure.
      else
        # Moved successfully (1 or 2)
        if status == 2
           # Found Oxygen System! We still continue exploring to map everything.
           # We store it in a global/outer variable (or return it)
        end

        # Recurse from new position
        res = explore(next_pos, computer, grid, visited)
        
        # Backtrack: Move robot back to current (x, y)
        rev_cmd = REVERSE_DIRS[cmd]
        computer.input(rev_cmd)
        back_status = computer.run_until_output
        # back_status should be 1 (or 2 if we stepped back onto oxygen, but logically just non-zero)
        raise "Backtrack failed" if back_status == 0
      end
    end
  end
end

# 1. Map the entire area
puts "Mapping the area..."
explore({0, 0}, computer, grid, visited)

# Find the oxygen system location from the grid
oxygen_locs = grid.select { |k, v| v == 2 }.keys
if oxygen_locs.empty?
  puts "Error: Oxygen system not found!"
  exit
end
start_node = oxygen_locs.first
puts "Oxygen System found at: #{start_node}"

# 2. Simulate Oxygen Fill (BFS)
# Find the distance to the furthest reachable point from the oxygen system
puts "Simulating oxygen spread..."

queue = Deque({Point, Int32}).new
queue << {start_node, 0}

# Track filled locations to avoid loops
filled = Set(Point).new
filled.add(start_node)

max_minutes = 0

while !queue.empty?
  current_pos, minutes = queue.shift
  max_minutes = minutes if minutes > max_minutes

  cx, cy = current_pos
  
  DIRS.each_value do |(dx, dy)|
    nx, ny = cx + dx, cy + dy
    neighbor = {nx, ny}

    # Oxygen spreads to open tiles (1) (and technically start pos which is 2)
    # It cannot spread to walls (0) or unknown (nil)
    if grid.has_key?(neighbor) && grid[neighbor] != 0 && !filled.includes?(neighbor)
      filled.add(neighbor)
      queue << {neighbor, minutes + 1}
    end
  end
end

puts "Time to fill with oxygen: #{max_minutes} minutes"
