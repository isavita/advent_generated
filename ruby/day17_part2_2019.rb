require 'set'

class IntcodeComputer
  def initialize(memory)
    @memory = Hash.new(0)
    memory.each_with_index { |val, i| @memory[i] = val }
    @pointer = 0
    @relative_base = 0
    @inputs = []
    @outputs = []
    @halted = false
  end

  attr_reader :outputs, :halted

  def get_param(mode, param)
    case mode
    when 0  # Position mode
      @memory[param]
    when 1  # Immediate mode
      param
    when 2  # Relative mode
      @memory[@relative_base + param]
    else
      raise "Unknown parameter mode: #{mode}"
    end
  end

  def set_param(mode, param, value)
    case mode
    when 0  # Position mode
      @memory[param] = value
    when 2  # Relative mode
      @memory[@relative_base + param] = value
    else
      raise "Unknown parameter mode for writing: #{mode}"
    end
  end

  def add_input(value)
    @inputs << value
  end

  def run
    loop do
      instruction = @memory[@pointer]
      opcode = instruction % 100
      modes = [
        (instruction / 100) % 10,
        (instruction / 1000) % 10,
        (instruction / 10000) % 10
      ]

      case opcode
      when 1  # Addition
        param1 = @memory[@pointer + 1]
        param2 = @memory[@pointer + 2]
        param3 = @memory[@pointer + 3]
        val1 = get_param(modes[0], param1)
        val2 = get_param(modes[1], param2)
        set_param(modes[2], param3, val1 + val2)
        @pointer += 4
      when 2  # Multiplication
        param1 = @memory[@pointer + 1]
        param2 = @memory[@pointer + 2]
        param3 = @memory[@pointer + 3]
        val1 = get_param(modes[0], param1)
        val2 = get_param(modes[1], param2)
        set_param(modes[2], param3, val1 * val2)
        @pointer += 4
      when 3  # Input
        return if @inputs.empty?
        param1 = @memory[@pointer + 1]
        input_value = @inputs.shift
        set_param(modes[0], param1, input_value)
        @pointer += 2
      when 4  # Output
        param1 = @memory[@pointer + 1]
        output_value = get_param(modes[0], param1)
        @outputs << output_value
        @pointer += 2
      when 5  # Jump-if-true
        param1 = @memory[@pointer + 1]
        param2 = @memory[@pointer + 2]
        val1 = get_param(modes[0], param1)
        val2 = get_param(modes[1], param2)
        @pointer = val1 != 0 ? val2 : @pointer + 3
      when 6  # Jump-if-false
        param1 = @memory[@pointer + 1]
        param2 = @memory[@pointer + 2]
        val1 = get_param(modes[0], param1)
        val2 = get_param(modes[1], param2)
        @pointer = val1 == 0 ? val2 : @pointer + 3
      when 7  # Less than
        param1 = @memory[@pointer + 1]
        param2 = @memory[@pointer + 2]
        param3 = @memory[@pointer + 3]
        val1 = get_param(modes[0], param1)
        val2 = get_param(modes[1], param2)
        set_param(modes[2], param3, val1 < val2 ? 1 : 0)
        @pointer += 4
      when 8  # Equals
        param1 = @memory[@pointer + 1]
        param2 = @memory[@pointer + 2]
        param3 = @memory[@pointer + 3]
        val1 = get_param(modes[0], param1)
        val2 = get_param(modes[1], param2)
        set_param(modes[2], param3, val1 == val2 ? 1 : 0)
        @pointer += 4
      when 9  # Adjust relative base
        param1 = @memory[@pointer + 1]
        val1 = get_param(modes[0], param1)
        @relative_base += val1
        @pointer += 2
      when 99  # Halt
        @halted = true
        return
      else
        raise "Unknown opcode: #{opcode}"
      end
    end
  end
end

def read_input(filename)
  File.read(filename).strip.split(',').map(&:to_i)
end

def parse_map(output)
  grid = []
  line = []
  output.each do |c|
    if c == 10
      grid << line.dup unless line.empty?
      line = []
    else
      line << c.chr
    end
  end
  grid << line unless line.empty?
  grid
end

def find_intersections(grid)
  intersections = []
  (1..grid.length - 2).each do |y|
    (1..grid[0].length - 2).each do |x|
      next unless grid[y][x] == '#'
      if grid[y-1][x] == '#' && grid[y+1][x] == '#' &&
         grid[y][x-1] == '#' && grid[y][x+1] == '#'
        intersections << [x, y]
      end
    end
  end
  intersections
end

def find_robot_position(grid)
  grid.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      return [x, y, cell] if ['^', 'v', '<', '>', 'X'].include?(cell)
    end
  end
  nil
end

def turn_left(direction)
  {'^' => '<', '<' => 'v', 'v' => '>', '>' => '^'}[direction]
end

def turn_right(direction)
  {'^' => '>', '>' => 'v', 'v' => '<', '<' => '^'}[direction]
end

def move_forward(x, y, direction)
  case direction
  when '^' then [x, y - 1]
  when 'v' then [x, y + 1]
  when '<' then [x - 1, y]
  when '>' then [x + 1, y]
  else
    raise "Unknown direction: #{direction}"
  end
end

def get_movement_path(grid, start_x, start_y, start_dir)
  x, y, direction = start_x, start_y, start_dir
  path = []
  steps = 0

  loop do
    next_x, next_y = move_forward(x, y, direction)
    if (0...grid.length).include?(next_y) && 
       (0...grid[0].length).include?(next_x) && 
       grid[next_y][next_x] == '#'
      x, y = next_x, next_y
      steps += 1
    else
      if steps > 0
        path << steps.to_s
        steps = 0
      end

      left_dir = turn_left(direction)
      next_x, next_y = move_forward(x, y, left_dir)
      if (0...grid.length).include?(next_y) && 
         (0...grid[0].length).include?(next_x) && 
         grid[next_y][next_x] == '#'
        path << 'L'
        direction = left_dir
        next
      end

      right_dir = turn_right(direction)
      next_x, next_y = move_forward(x, y, right_dir)
      if (0...grid.length).include?(next_y) && 
         (0...grid[0].length).include?(next_x) && 
         grid[next_y][next_x] == '#'
        path << 'R'
        direction = right_dir
        next
      end

      break
    end
  end
  path
end

def compress_movement(path)
  def is_valid_routine(routine)
    routine.length <= 20
  end

  def replace_sequence(seq, pattern, replacement)
    i = 0
    res = []
    while i < seq.length
      if seq[i, pattern.length] == pattern
        res << replacement
        i += pattern.length
      else
        res << seq[i]
        i += 1
      end
    end
    res
  end

  path_str = path.join(',')
  tokens = path_str.split(',')

  max_function_length = 20
  max_pattern_length = 10

  (1..max_pattern_length).each do |a_len|
    a_pattern = tokens[0, a_len]
    a_str = a_pattern.join(',')
    next if a_str.length > max_function_length

    tokens_after_a = replace_sequence(tokens, a_pattern, 'A')

    (a_len...tokens.length).each do |b_start|
      (1..max_pattern_length).each do |b_len|
        b_end = b_start + b_len
        b_pattern = tokens[b_start, b_len]
        b_str = b_pattern.join(',')
        next if b_str.length > max_function_length

        tokens_after_b = replace_sequence(tokens_after_a, b_pattern, 'B')

        (b_end...tokens.length).each do |c_start|
          (1..max_pattern_length).each do |c_len|
            c_pattern = tokens[c_start, c_len]
            c_str = c_pattern.join(',')
            next if c_str.length > max_function_length

            tokens_after_c = replace_sequence(tokens_after_b, c_pattern, 'C')
            main_tokens = tokens_after_c.dup

            loop do
              changed = false
              temp_tokens = main_tokens.dup
              main_tokens = []
              i = 0
              while i < temp_tokens.length
                if temp_tokens[i, a_pattern.length] == a_pattern
                  main_tokens << 'A'
                  i += a_pattern.length
                  changed = true
                elsif temp_tokens[i, b_pattern.length] == b_pattern
                  main_tokens << 'B'
                  i += b_pattern.length
                  changed = true
                elsif temp_tokens[i, c_pattern.length] == c_pattern
                  main_tokens << 'C'
                  i += c_pattern.length
                  changed = true
                else
                  main_tokens << temp_tokens[i]
                  i += 1
                end
              end
              break unless changed
            end

            main_routine = main_tokens.join(',')
            if main_routine.match?(/^[ABC,]+$/) && main_routine.length <= 20
              function_a = a_pattern.join(',')
              function_b = b_pattern.join(',')
              function_c = c_pattern.join(',')
              if [function_a, function_b, function_c].all? { |f| f.length <= 20 }
                return [main_routine, function_a, function_b, function_c]
              end
            end
          end
        end
      end
    end
  end
  raise "Could not compress the path into functions A, B, C."
end

def main
  # Read Intcode program
  program = read_input('input.txt')

  # Part One
  computer = IntcodeComputer.new(program.dup)
  computer.run
  output = computer.outputs
  grid = parse_map(output)

  intersections = find_intersections(grid)
  alignment_sum = intersections.sum { |x, y| x * y }
  puts "Part One: Sum of alignment parameters = #{alignment_sum}"

  # Part Two
  program_part2 = program.dup
  program_part2[0] = 2
  computer_part2 = IntcodeComputer.new(program_part2)

  robot = find_robot_position(grid)
  raise "Robot not found on the scaffold." unless robot
  start_x, start_y, start_dir = robot

  movement_path = get_movement_path(grid, start_x, start_y, start_dir)

  begin
    main_routine, function_a, function_b, function_c = compress_movement(movement_path)
  rescue => e
    puts "Error in compressing path: #{e}"
    exit 1
  end

  input_lines = [
    main_routine,
    function_a,
    function_b,
    function_c,
    'n'
  ]

  input_lines.each do |line|
    line.each_char { |char| computer_part2.add_input(char.ord) }
    computer_part2.add_input(10)  # Newline
  end

  computer_part2.run while !computer_part2.halted

  dust_collected = computer_part2.outputs.last
  puts "Part Two: Dust collected = #{dust_collected}"
end

main if __FILE__ == $PROGRAM_NAME
