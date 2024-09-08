class Module
  attr_reader :name, :type, :destinations
  attr_accessor :state, :inputs

  def initialize(name, type, destinations)
    @name = name
    @type = type
    @destinations = destinations
    @state = false
    @inputs = {}
  end
end

def parse_input(filename)
  modules = {}
  File.readlines(filename).each do |line|
    name, destinations = line.strip.split(' -> ')
    type = name[0]
    name = name[1..-1] if type == '%' || type == '&'
    name = 'broadcaster' if type == 'b'
    modules[name] = Module.new(name, type, destinations.split(', '))
  end

  modules.each do |name, mod|
    mod.destinations.each do |dest|
      modules[dest].inputs[name] = false if modules[dest]
    end
  end

  modules
end

def process_pulse(modules, queue, low_count, high_count)
  source, target, pulse = queue.shift
  pulse == :low ? low_count[0] += 1 : high_count[0] += 1

  return unless modules[target]

  case modules[target].type
  when 'b'
    modules[target].destinations.each { |dest| queue << [target, dest, pulse] }
  when '%'
    if pulse == :low
      modules[target].state = !modules[target].state
      new_pulse = modules[target].state ? :high : :low
      modules[target].destinations.each { |dest| queue << [target, dest, new_pulse] }
    end
  when '&'
    modules[target].inputs[source] = pulse == :high
    new_pulse = modules[target].inputs.values.all? ? :low : :high
    modules[target].destinations.each { |dest| queue << [target, dest, new_pulse] }
  end
end

def simulate_button_press(modules, low_count, high_count)
  queue = [['button', 'broadcaster', :low]]
  queue.each { |pulse| process_pulse(modules, queue, low_count, high_count) } until queue.empty?
end

modules = parse_input('input.txt')
low_count, high_count = [0], [0]

1000.times { simulate_button_press(modules, low_count, high_count) }

puts low_count[0] * high_count[0]
