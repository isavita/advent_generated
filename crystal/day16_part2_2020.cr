
struct Rule
  property name : String
  property ranges : Array(Tuple(Int32, Int32))

  def initialize(@name : String, @ranges : Array(Tuple(Int32, Int32)))
  end

  def valid?(value : Int32) : Bool
    @ranges.any? { |rng| value >= rng[0] && value <= rng[1] }
  end
end

def to_i(s : String) : Int32
  s.to_i
end

def parse_ticket(s : String) : Array(Int32)
  s.split(",").map(&.to_i)
end

def valid_ticket?(ticket : Array(Int32), rules : Array(Rule)) : Bool
  ticket.all? { |value| valid_for_any_rule?(value, rules) }
end

def valid_for_any_rule?(value : Int32, rules : Array(Rule)) : Bool
  rules.any? { |rule| rule.valid?(value) }
end

def solve_field_positions(rules : Array(Rule), tickets : Array(Array(Int32))) : Hash(String, Int32)
  valid_positions = Hash(String, Hash(Int32, Bool)).new
  rules.each do |rule|
    valid_positions[rule.name] = Hash(Int32, Bool).new
    tickets[0].size.times do |i|
      valid = tickets.all? { |ticket| rule.valid?(ticket[i]) }
      valid_positions[rule.name][i] = true if valid
    end
  end

  field_positions = Hash(String, Int32).new
  while field_positions.size < rules.size
    valid_positions.each do |name, positions|
      if positions.size == 1
        pos = positions.keys.first
        field_positions[name] = pos
        valid_positions.each_value { |other_positions| other_positions.delete(pos) }
        valid_positions.delete(name)
      end
    end
  end
  field_positions
end

def calculate_departure_product(ticket : Array(Int32), field_positions : Hash(String, Int32)) : Int64
  product = 1_i64
  field_positions.each do |name, pos|
    if name.starts_with?("departure")
      product *= ticket[pos].to_i64
    end
  end
  product
end

file = File.open("input.txt")
rules = Array(Rule).new
my_ticket = Array(Int32).new
nearby_tickets = Array(Array(Int32)).new
section = 0
re_rule = /(.+): (\d+)-(\d+) or (\d+)-(\d+)/

file.each_line do |line|
  line = line.strip
  if line.empty?
    section += 1
    next
  end
  case section
  when 0
    if match = re_rule.match(line)
      rule = Rule.new(
        match[1].to_s,
        [
          {to_i(match[2]), to_i(match[3])},
          {to_i(match[4]), to_i(match[5])},
        ]
      )
      rules << rule
    end
  when 1
    if line != "your ticket:"
      my_ticket = parse_ticket(line)
    end
  when 2
    if line != "nearby tickets:"
      ticket = parse_ticket(line)
      nearby_tickets << ticket if valid_ticket?(ticket, rules)
    end
  end
end

field_positions = solve_field_positions(rules, nearby_tickets)
departure_product = calculate_departure_product(my_ticket, field_positions)

puts departure_product
