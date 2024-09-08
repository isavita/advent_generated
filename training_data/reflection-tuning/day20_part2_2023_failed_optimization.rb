class Module
  attr_reader :name, :type, :destinations, :state, :inputs

  def initialize(name, type, destinations)
    @name = name
    @type = type
    @destinations = destinations
    @state = false
    @inputs = {}
  end

  def receive_pulse(source, pulse)
    case @type
    when '%'
      return [] if pulse == :high
      @state = !@state
      return @destinations.map { |dest| [dest, @state ? :high : :low, @name] }
    when '&'
      @inputs[source] = pulse
      output = @inputs.values.all? { |p| p == :high } ? :low : :high
      return @destinations.map { |dest| [dest, output, @name] }
    else
      return @destinations.map { |dest| [dest, pulse, @name] }
    end
  end
end

def parse_input(input)
  modules = {}
  input.each_line do |line|
    name, destinations = line.strip.split(' -> ')
    type = name[0]
    name = name[1..-1] if type =~ /[%&]/
    modules[name] = Module.new(name, type, destinations.split(', '))
  end

  modules.each do |name, mod|
    mod.destinations.each do |dest|
      modules[dest].inputs[name] = :low if modules[dest]
    end
  end

  modules
end

def simulate_button_press(modules)
  queue = [['broadcaster', :low, 'button']]
  low_count = 1
  high_count = 0

  until queue.empty?
    dest, pulse, source = queue.shift
    if pulse == :low
      low_count += 1
    else
      high_count += 1
    end

    next unless modules[dest]
    queue.concat(modules[dest].receive_pulse(source, pulse))
  end

  [low_count, high_count]
end

def find_cycle(modules)
  states = {}
  total_low = 0
  total_high = 0

  (1..1000).each do |i|
    low, high = simulate_button_press(modules)
    total_low += low
    total_high += high

    state = modules.values.map(&:state).join
    if states[state]
      cycle_length = i - states[state]
      remaining = 1000 - i
      full_cycles = remaining / cycle_length
      total_low += full_cycles * (total_low - states["low_#{state}"])
      total_high += full_cycles * (total_high - states["high_#{state}"])
      break
    end

    states[state] = i
    states["low_#{state}"] = total_low
    states["high_#{state}"] = total_high
  end

  [total_low, total_high]
end

def find_rx_cycle(modules)
  rx_input = modules.values.find { |m| m.destinations.include?('rx') }
  cycles = rx_input.inputs.keys.map { |input| find_input_cycle(modules, input) }
  cycles.reduce(1, :lcm)
end

def find_input_cycle(modules, target)
  count = 0
  loop do
    count += 1
    queue = [['broadcaster', :low, 'button']]
    until queue.empty?
      dest, pulse, source = queue.shift
      return count if dest == target && pulse == :high
      next unless modules[dest]
      queue.concat(modules[dest].receive_pulse(source, pulse))
    end
  end
end

input = File.read('input.txt')
modules = parse_input(input)

# Part One
low, high = find_cycle(modules)
puts "Part One: #{low * high}"

# Part Two
modules = parse_input(input)  # Reset modules
puts "Part Two: #{find_rx_cycle(modules)}"
