class Scanner
  property range : Int32, position : Int32, direction : Int32

  def initialize(@range : Int32, @position : Int32 = 0, @direction : Int32 = 1)
  end

  def move
    if @position == 0
      @direction = 1
    elsif @position == @range - 1
      @direction = -1
    end
    @position += @direction
  end
end

firewall = {} of Int32 => Scanner
File.each_line("input.txt") do |line|
  depth, range = line.split(": ").map(&.to_i)
  firewall[depth] = Scanner.new(range)
end

max_depth = firewall.keys.max
severity = 0

(0..max_depth).each do |depth|
  if scanner = firewall[depth]?
    severity += depth * scanner.range if scanner.position == 0
  end

  firewall.each_value(&.move)
end

puts severity