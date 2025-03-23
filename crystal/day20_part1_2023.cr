
enum PulseValue
  Low
  High
end

class Pulse
  property value : PulseValue
  property from_name : String
  property to_name : String

  def initialize(value : PulseValue, from_name : String, to_name : String)
    @value = value
    @from_name = from_name
    @to_name = to_name
  end
end

enum ModulePrefix
  FlipFlop
  Conjunction
  Broadcast
  None
end

class Module
  property name : String
  property prefix : ModulePrefix
  property destinations : Array(String)
  property state : Bool = false
  property memory : Hash(String, PulseValue) = Hash(String, PulseValue).new

  def initialize(name : String, prefix : ModulePrefix, destinations : Array(String))
    @name = name
    @prefix = prefix
    @destinations = destinations
  end
end

def parse_input(input : Array(String)) : Hash(String, Module)
  modules = Hash(String, Module).new

  input.each do |line|
    parts = line.split(" -> ")
    module_name_part = parts[0]
    destinations = parts[1].split(", ")

    prefix = case module_name_part[0]
             when '%'
               ModulePrefix::FlipFlop
             when '&'
               ModulePrefix::Conjunction
             when 'b'
               ModulePrefix::Broadcast
             else
               ModulePrefix::None
             end

    module_name = case prefix
                  when ModulePrefix::FlipFlop, ModulePrefix::Conjunction
                    module_name_part[1..]
                  else
                    module_name_part
                  end

    module_ = Module.new(module_name, prefix, destinations)
    modules[module_.name] = module_
  end

  modules.each do |_, module_|
    module_.destinations.each do |dest_name|
      if modules.has_key?(dest_name) && modules[dest_name].prefix == ModulePrefix::Conjunction
        modules[dest_name].memory[module_.name] = PulseValue::Low
      end
    end
  end

  modules
end

def push_button(modules : Hash(String, Module), start_pulse : Pulse, num_cycles : Int32) : Tuple(Int64, Int64)
  cnt_low = 0_i64
  cnt_high = 0_i64

  num_cycles.times do
    pulse_queue = [start_pulse]

    while !pulse_queue.empty?
      pulse = pulse_queue.shift

      case pulse.value
      when PulseValue::Low
        cnt_low += 1
      when PulseValue::High
        cnt_high += 1
      end

      next unless modules.has_key?(pulse.to_name)

      module_ = modules[pulse.to_name]

      new_pulse_value : PulseValue? = nil
      case module_.prefix
      when ModulePrefix::FlipFlop
        if pulse.value == PulseValue::Low
          module_.state = !module_.state
          new_pulse_value = module_.state ? PulseValue::High : PulseValue::Low
        else
          next
        end
      when ModulePrefix::Conjunction
        module_.memory[pulse.from_name] = pulse.value
        is_high_for_all = module_.memory.values.all? { |value| value == PulseValue::High }
        new_pulse_value = is_high_for_all ? PulseValue::Low : PulseValue::High
      when ModulePrefix::Broadcast
        new_pulse_value = pulse.value
      when ModulePrefix::None
        new_pulse_value = pulse.value
      end

      next if new_pulse_value.nil?

      module_.destinations.each do |dest_name|
        new_pulse = Pulse.new(new_pulse_value, module_.name, dest_name)
        pulse_queue << new_pulse
      end
    end
  end

  {cnt_low, cnt_high}
end

def solve(input : Array(String)) : Int64
  start_pulse = Pulse.new(PulseValue::Low, "button", "broadcaster")
  num_cycles = 1000

  modules = parse_input(input)

  cnt_low, cnt_high = push_button(modules, start_pulse, num_cycles)

  cnt_low * cnt_high
end

def read_file(file_name : String) : Array(String)
  File.read_lines(file_name)
end

def main
  input = read_file("input.txt")
  puts solve(input)
end

main
