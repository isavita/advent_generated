class Broadcaster
  attr_accessor :name, :connects_to

  def initialize(name, connects_to)
    @name = name
    @connects_to = connects_to
  end
end

class FlipFlop
  attr_accessor :name, :connects_to, :state

  def initialize(name, connects_to)
    @name = name
    @connects_to = connects_to
    @state = false
  end
end

class Conjunction
  attr_accessor :name, :connects_to, :watches

  def initialize(name, connects_to)
    @name = name
    @connects_to = connects_to
    @watches = {}
  end
end

def handle_line(line, connections)
  if line.include?("broadcaster")
    parts = line.split(" -> ")
    name = parts[0]
    connects_to = parts[1].split(", ")
    connections[name] = Broadcaster.new(name, connects_to)
  elsif line.include?("%")
    parts = line.split(" -> ")
    name = parts[0][1..-1]
    connects_to = parts[1].split(", ")
    connections[name] = FlipFlop.new(name, connects_to)
  else
    parts = line.split(" -> ")
    name = parts[0][1..-1]
    connects_to = parts[1].split(", ")
    connections[name] = Conjunction.new(name, connects_to)
  end
end

def complete_watches(connections)
  connections.each_value do |module_obj|
    if module_obj.is_a?(Conjunction)
      connections.each_value do |other|
        other.connects_to.each do |name|
          module_obj.watches[other.name] = false if name == module_obj.name
        end
      end
    end
  end
end

def simulate_press(connections, loops, press_number)
  queue = [{ from: 'button', name: 'broadcaster', pulse: false }]
  pulses = [1, 0]
  found = false

  until queue.empty?
    curr_state = queue.shift
    module_obj = connections[curr_state[:name]]

    next if curr_state[:name] == 'out'

    found = true if curr_state[:name] == 'rx' && !curr_state[:pulse]

    pulse = curr_state[:pulse]

    case module_obj
    when Broadcaster
      module_obj.connects_to.each do |name|
        queue.push({ from: module_obj.name, name: name, pulse: pulse })
        pulse ? pulses[1] += 1 : pulses[0] += 1
      end
    when FlipFlop
      unless pulse
        module_obj.state = !module_obj.state
        module_obj.connects_to.each do |name|
          queue.push({ from: module_obj.name, name: name, pulse: module_obj.state })
          module_obj.state ? pulses[1] += 1 : pulses[0] += 1
        end
      end
    when Conjunction
      module_obj.watches[curr_state[:from]] = pulse
      all_true = module_obj.watches.values.all?

      module_obj.connects_to.each do |name|
        queue.push({ from: module_obj.name, name: name, pulse: !all_true })
        !all_true ? pulses[1] += 1 : pulses[0] += 1
      end

      if loops.key?(curr_state[:name]) && !all_true && loops[curr_state[:name]] == -1
        loops[curr_state[:name]] = press_number
      end
    end
  end

  [pulses, found]
end

connections = {}
File.foreach("input.txt") do |line|
  handle_line(line.strip, connections)
end

complete_watches(connections)

px_prev = connections.select { |k, v| v.connects_to.include?('rx') }.keys
raise "Error: more than one pxPrev" unless px_prev.size == 1

conj = connections[px_prev[0]]
raise "Error: pxPrev is not a conjunction" unless conj.is_a?(Conjunction)

loop_lengths = conj.watches.keys.map { |name| [name, -1] }.to_h

press_number = 0
loop do
  press_number += 1
  _, found = simulate_press(connections, loop_lengths, press_number)
  break if found || loop_lengths.values.all? { |length| length != -1 }
end

sum = loop_lengths.values.reduce(1, :*)

puts sum