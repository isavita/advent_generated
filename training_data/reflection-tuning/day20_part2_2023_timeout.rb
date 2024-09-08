class Module
  attr_reader :name, :type, :destinations

  def initialize(name, type, destinations)
    @name = name
    @type = type
    @destinations = destinations
    @state = false
    @memory = {}
  end

  def receive(pulse, from)
    case @type
    when '%'
      return [] if pulse == :high
      @state = !@state
      return @destinations.map { |dest| [@name, dest, @state ? :high : :low] }
    when '&'
      @memory[from] = pulse
      output = @memory.values.all?(:high) ? :low : :high
      return @destinations.map { |dest| [@name, dest, output] }
    else
      return @destinations.map { |dest| [@name, dest, pulse] }
    end
  end
end

def parse_input(filename)
  modules = {}
  File.readlines(filename, chomp: true).each do |line|
    name, destinations = line.split(' -> ')
    type = name[0]
    name = name[1..-1] if type =~ /[%&]/
    modules[name] = Module.new(name, type, destinations.split(', '))
  end
  modules
end

def simulate(modules, target = nil)
  low_count = high_count = 0
  button_presses = 0

  loop do
    button_presses += 1
    queue = [['button', 'broadcaster', :low]]

    while !queue.empty?
      from, to, pulse = queue.shift
      return button_presses if target && to == target && pulse == :low

      pulse == :low ? low_count += 1 : high_count += 1

      next unless modules[to]
      queue.concat(modules[to].receive(pulse, from))
    end

    break if target.nil? && button_presses == 1000
  end

  [low_count, high_count]
end

modules = parse_input('input.txt')

# Part 1
low, high = simulate(modules)
puts "Part 1: #{low * high}"

# Part 2
puts "Part 2: #{simulate(modules, 'rx')}"
