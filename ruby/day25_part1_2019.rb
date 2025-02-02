#!/usr/bin/env ruby
require 'set'

class Room
  attr_reader :name, :connections
  
  def initialize(name)
    @name = name
    @connections = {}
  end
end

module Mode
  EXPLORE = 0
  NAVIGATE = 1
  TEST = 2
end

OPPOSITE = {
  "north" => "south",
  "south" => "north",
  "west" => "east",
  "east" => "west"
}

module EmulatorStatus
  HALTED = 0
  OUTPUT = 1
  WAITING_FOR_INPUT = 2
end

class Emulator
  def initialize(program, input_values = nil)
    # Convert all numbers to integers and store as a hash with default 0
    @memory = Hash.new(0)
    program.each_with_index { |val, i| @memory[i] = val.to_i }
    @input = input_values ? input_values.map(&:to_i) : []
    @ip = 0
    @relative_base = 0
  end

  def write_string(s)
    s.each_char { |char| @input.push(char.ord) }
    [s.length, nil]
  end

  def get_parameter(mode, param, instruction)
    case mode
    when 0  # Position mode
      @memory[param.to_i]
    when 1  # Immediate mode
      param.to_i
    when 2  # Relative mode
      @memory[(@relative_base + param.to_i)]
    else
      raise "Unknown parameter mode: #{mode}"
    end
  end

  def get_write_address(mode, param)
    case mode
    when 0  # Position mode
      param.to_i
    when 2  # Relative mode
      (@relative_base + param.to_i)
    else
      raise "Invalid mode for writing: #{mode}"
    end
  end

  def emulate
    loop do
      instruction = @memory[@ip].to_i
      opcode = instruction % 100

      modes = [
        (instruction / 100) % 10,
        (instruction / 1000) % 10,
        (instruction / 10000) % 10
      ].map(&:to_i)

      case opcode
      when 1  # Addition
        param1 = get_parameter(modes[0], @memory[@ip + 1], instruction)
        param2 = get_parameter(modes[1], @memory[@ip + 2], instruction)
        dest = get_write_address(modes[2], @memory[@ip + 3])
        @memory[dest] = param1 + param2
        @ip += 4
      when 2  # Multiplication
        param1 = get_parameter(modes[0], @memory[@ip + 1], instruction)
        param2 = get_parameter(modes[1], @memory[@ip + 2], instruction)
        dest = get_write_address(modes[2], @memory[@ip + 3])
        @memory[dest] = param1 * param2
        @ip += 4
      when 3  # Input
        return [nil, EmulatorStatus::WAITING_FOR_INPUT] if @input.empty?
        dest = get_write_address(modes[0], @memory[@ip + 1])
        @memory[dest] = @input.shift
        @ip += 2
      when 4  # Output
        param = get_parameter(modes[0], @memory[@ip + 1], instruction)
        @ip += 2
        return [param, EmulatorStatus::OUTPUT]
      when 5  # Jump if true
        param1 = get_parameter(modes[0], @memory[@ip + 1], instruction)
        param2 = get_parameter(modes[1], @memory[@ip + 2], instruction)
        @ip = param1 != 0 ? param2 : @ip + 3
      when 6  # Jump if false
        param1 = get_parameter(modes[0], @memory[@ip + 1], instruction)
        param2 = get_parameter(modes[1], @memory[@ip + 2], instruction)
        @ip = param1 == 0 ? param2 : @ip + 3
      when 7  # Less than
        param1 = get_parameter(modes[0], @memory[@ip + 1], instruction)
        param2 = get_parameter(modes[1], @memory[@ip + 2], instruction)
        dest = get_write_address(modes[2], @memory[@ip + 3])
        @memory[dest] = param1 < param2 ? 1 : 0
        @ip += 4
      when 8  # Equals
        param1 = get_parameter(modes[0], @memory[@ip + 1], instruction)
        param2 = get_parameter(modes[1], @memory[@ip + 2], instruction)
        dest = get_write_address(modes[2], @memory[@ip + 3])
        @memory[dest] = param1 == param2 ? 1 : 0
        @ip += 4
      when 9  # Adjust relative base
        param = get_parameter(modes[0], @memory[@ip + 1], instruction)
        @relative_base += param
        @ip += 2
      when 99  # Halt
        return [nil, EmulatorStatus::HALTED]
      else
        raise "Unknown opcode: #{opcode} at position #{@ip}"
      end
    end
  end
end

def read_file(filename)
  File.read(filename).strip
end

def find_path(from_room, to_room)
  queue = [[from_room, [from_room]]]
  visited = Set.new([from_room.name])

  until queue.empty?
    current, path = queue.shift
    return path if current == to_room
    
    current.connections.each_value do |neighbor|
      if neighbor && !visited.include?(neighbor.name)
        visited.add(neighbor.name)
        queue.push([neighbor, path + [neighbor]])
      end
    end
  end
  nil
end

def main
  text = read_file("input.txt")
  program = text.split(",").map(&:to_i)
  emulator = Emulator.new(program)

  def send_command(emulator, format_str, *args)
    cmd = format_str % args
    emulator.write_string(cmd)
  end

  room_name_regex = /^== (.+) ==$/
  list_item_regex = /^- (.+)$/
  taken_regex = /^You take the (.+)\.$/
  dropped_regex = /^You drop the (.+)\.$/
  result_regex = /"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."$/

  world = {}
  inventory = {}
  mode = Mode::EXPLORE
  path = []
  checkpoint = nil
  floor = nil
  test_dir = ""
  available_items = []
  item_mask = 0
  last = nil
  last_items = []
  last_dir = ""
  output_builder = []
  current_room = nil

  loop do
    char, status = emulator.emulate

    case status
    when EmulatorStatus::HALTED
      output = output_builder.map(&:chr).join
      output.split("\n").each do |line|
        if match = result_regex.match(line)
          puts match[1]
          return
        end
      end

    when EmulatorStatus::OUTPUT
      output_builder.push(char) if char

    when EmulatorStatus::WAITING_FOR_INPUT
      output = output_builder.map(&:chr).join
      output_builder = []

      items = []
      lines = output.split("\n")
      i = 0
      while i < lines.length
        line = lines[i].strip

        if line.empty? || line == "Command?"
          i += 1
          next
        end

        if match = room_name_regex.match(line)
          name = match[1]
          i += 1
          while i < lines.length && !lines[i].strip.empty?
            i += 1
          end
          
          if !world[name]
            current_room = Room.new(name)
            world[name] = current_room
          else
            current_room = world[name]
          end
          items = []
          next
        end

        if line == "Doors here lead:"
          i += 1
          while i < lines.length && !lines[i].strip.empty?
            if match = list_item_regex.match(lines[i].strip)
              direction = match[1]
              current_room.connections[direction] ||= nil if current_room
            end
            i += 1
          end
          next
        end

        if line == "Items here:"
          i += 1
          while i < lines.length && !lines[i].strip.empty?
            if match = list_item_regex.match(lines[i].strip)
              items.push(match[1])
            end
            i += 1
          end
          next
        end

        if match = taken_regex.match(line)
          taken = match[1]
          inventory[taken] = true
          if last
            current_room = last
            items = last_items.reject { |item| item == taken }
          end
          i += 1
          next
        end

        if match = dropped_regex.match(line)
          dropped = match[1]
          inventory[dropped] = false
          if last
            current_room = last
            items = last_items + [dropped]
          end
          i += 1
          next
        end

        if line.start_with?('A loud, robotic voice says "Alert!')
          if mode == Mode::EXPLORE
            path.pop if path.any?
            checkpoint, floor, test_dir = last, current_room, last_dir
            checkpoint.connections[test_dir] = floor if checkpoint && test_dir
          end
          last, last_items, last_dir = nil, [], ""
          i += 1
          next
        end

        i += 1
      end

      if last && !last_dir.empty? && current_room
        if last.connections[last_dir].nil?
          last.connections[last_dir] = current_room
          current_room.connections[OPPOSITE[last_dir]] = last
        end
      end

      last, last_items, last_dir = current_room, items, ""

      case mode
      when Mode::EXPLORE
        blacklist = ["photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet"]
        item_to_take = items.find { |item| !blacklist.include?(item) }
        
        if item_to_take
          send_command(emulator, "take %s\n", item_to_take)
        else
          target = nil
          current_room.connections.each do |direction, room|
            if room.nil?
              path.push(current_room)
              target = direction
              break
            end
          end

          if target
            last_dir = target
            send_command(emulator, "%s\n", target)
            next
          end

          if path.any?
            last_room = path.pop
            back_dir = current_room.connections.key(last_room)
            if back_dir
              last_dir = back_dir
              send_command(emulator, "%s\n", back_dir)
              next
            else
              raise "Cannot go from '#{current_room.name}' to '#{last_room.name}'"
            end
          end

          if checkpoint && floor
            new_path = find_path(current_room, checkpoint)
            if new_path
              path = new_path[1..-1] || []
            end
            mode = Mode::NAVIGATE
            next
          end
        end

      when Mode::NAVIGATE
        if path.any?
          next_room = path.shift
          direction = current_room.connections.key(next_room)
          if direction
            last_dir = direction
            send_command(emulator, "%s\n", direction)
            next
          else
            raise "Cannot go from '#{current_room.name}' to '#{next_room.name}'"
          end
        else
          available_items = inventory.select { |_, has| has }.keys
          item_mask = 0
          mode = Mode::TEST
        end

      when Mode::TEST
        item_found = false
        available_items.each_with_index do |item, index|
          target_state = (item_mask & (1 << index)) != 0
          if inventory[item] != target_state
            action = target_state ? "take" : "drop"
            send_command(emulator, "%s %s\n", action, item)
            item_found = true
            break
          end
        end

        unless item_found
          item_mask += 1
          if test_dir.empty?
            raise "Test direction (test_dir) is not set."
          else
            send_command(emulator, "%s\n", test_dir)
            next
          end
        end
      end
    end
  end
end

main if __FILE__ == $PROGRAM_NAME
