
module ModuleType
  BROADCASTER = 0
  FLIPFLOP    = 1
  CONJUNCTION = 2
end

abstract class Module
  property name : String, connects_to : Array(String)
  def initialize(@name : String, @connects_to : Array(String)); end
end

class Broadcaster < Module
  def initialize(name : String, connects_to : Array(String))
    super(name, connects_to)
  end
end

class FlipFlop < Module
  property state : Bool
  def initialize(name : String, connects_to : Array(String), @state = false)
    super(name, connects_to)
  end
end

class Conjunction < Module
  property watches : Hash(String, Bool)
  def initialize(name : String, connects_to : Array(String), @watches = Hash(String, Bool).new)
    super(name, connects_to)
  end
end

record State, from : String, name : String, pulse : Bool

lines = File.read_lines("input.txt")
connections = Hash(String, Module).new

lines.each do |line|
  if line.includes?("broadcaster")
    _, rhs = line.split(" -> ")
    connections["broadcaster"] = Broadcaster.new("broadcaster", rhs.split(", "))
  elsif line.starts_with?('%')
    lhs, rhs = line.split(" -> ")
    name = lhs[1..]
    connections[name] = FlipFlop.new(name, rhs.split(", "))
  else
    lhs, rhs = line.split(" -> ")
    name = lhs[1..]
    connections[name] = Conjunction.new(name, rhs.split(", "))
  end
end

connections.each_value do |mod|
  if mod.is_a?(Conjunction)
    connections.each_value do |other|
      other.connects_to.each do |dst|
        mod.watches[other.name] = false if dst == mod.name
      end
    end
  end
end

loop_lengths = Hash(String, Int32).new
px_prev = connections.find { |_, v| v.connects_to.includes?("rx") }.not_nil![0]
conj = connections[px_prev].as(Conjunction)
conj.watches.each_key { |k| loop_lengths[k] = -1 }

press_number = 0
loop do
  press_number += 1
  break if simulate_press(connections, loop_lengths, press_number)
  break if loop_lengths.values.all? { |v| v != -1 }
end

puts loop_lengths.values.product(1_i64)

def simulate_press(connections, loops, press_number) : Bool
  queue = Deque{State.new("button", "broadcaster", false)}
  until queue.empty?
    st = queue.shift
    mod = connections[st.name]?

    next if st.name == "out"
    return true if st.name == "rx" && !st.pulse

    pulse = st.pulse

    case mod
    when Broadcaster
      mod.connects_to.each do |n|
        queue << State.new(mod.name, n, pulse)
      end
    when FlipFlop
      unless pulse
        mod.state = !mod.state
        mod.connects_to.each do |n|
          queue << State.new(mod.name, n, mod.state)
        end
      end
    when Conjunction
      mod.watches[st.from] = pulse
      all_true = mod.watches.values.all?
      mod.connects_to.each do |n|
        queue << State.new(mod.name, n, !all_true)
      end
      if loops.has_key?(mod.name) && !all_true && loops[mod.name] == -1
        loops[mod.name] = press_number
      end
    end
  end
  false
end
