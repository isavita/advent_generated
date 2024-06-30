class Module
  attr_reader :name, :destinations

  def initialize(name, destinations)
    @name = name
    @destinations = destinations
  end

  def receive_pulse(pulse, _source)
    @destinations.map { |dest| [dest, pulse, @name] }
  end
end

class Broadcaster < Module
  def receive_pulse(pulse, _source)
    super
  end
end

class FlipFlop < Module
  def initialize(name, destinations)
    super
    @state = false
  end

  def receive_pulse(pulse, _source)
    return [] if pulse == :high
    @state = !@state
    super(@state ? :high : :low, _source)
  end
end

class Conjunction < Module
  def initialize(name, destinations)
    super
    @memory = {}
  end

  def add_input(input)
    @memory[input] = :low
  end

  def receive_pulse(pulse, source)
    @memory[source] = pulse
    output_pulse = @memory.values.all? { |p| p == :high } ? :low : :high
    super(output_pulse, source)
  end
end

def parse_input(input)
  modules = {}
  conjunctions = []

  input.each_line do |line|
    name, destinations = line.strip.split(' -> ')
    destinations = destinations.split(', ')

    case name[0]
    when '%'
      modules[name[1..-1]] = FlipFlop.new(name[1..-1], destinations)
    when '&'
      conj = Conjunction.new(name[1..-1], destinations)
      modules[name[1..-1]] = conj
      conjunctions << conj
    when 'b'
      modules[name] = Broadcaster.new(name, destinations)
    end
  end

  # Set up inputs for conjunction modules
  modules.each do |name, mod|
    mod.destinations.each do |dest|
      if modules[dest].is_a?(Conjunction)
        modules[dest].add_input(name)
      end
    end
  end

  modules
end

def simulate(modules, button_presses)
  low_pulses = 0
  high_pulses = 0

  button_presses.times do
    queue = [['broadcaster', :low, 'button']]

    while !queue.empty?
      dest, pulse, source = queue.shift

      if pulse == :low
        low_pulses += 1
      else
        high_pulses += 1
      end

      if modules[dest]
        new_pulses = modules[dest].receive_pulse(pulse, source)
        queue.concat(new_pulses)
      end
    end
  end

  [low_pulses, high_pulses]
end

# Read input from file
input = File.read('input.txt')

# Parse input and create modules
modules = parse_input(input)

# Simulate 1000 button presses
low_pulses, high_pulses = simulate(modules, 1000)

# Calculate and print the result
result = low_pulses * high_pulses
puts "Low pulses: #{low_pulses}"
puts "High pulses: #{high_pulses}"
puts "Result (low * high): #{result}"
