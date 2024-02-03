
def parse_input(file)
  sections = File.read(file).split("\n\n")
  rules = sections[0].split("\n").map do |line|
    field, ranges = line.split(": ")
    {
      field: field,
      ranges: ranges.split(" or ").map { |range| range.split("-").map(&:to_i) }
    }
  end
  my_ticket = sections[1].split("\n")[1].split(",").map(&:to_i)
  nearby_tickets = sections[2].split("\n")[1..].map { |line| line.split(",").map(&:to_i) }
  [rules, my_ticket, nearby_tickets]
end

def valid_value?(value, rules)
  rules.any? { |rule| rule[:ranges].any? { |(min, max)| value.between?(min, max) } }
end

def part_one(rules, nearby_tickets)
  nearby_tickets.flatten.select { |value| !valid_value?(value, rules) }.sum
end

def valid_ticket?(ticket, rules)
  ticket.all? { |value| valid_value?(value, rules) }
end

def determine_fields(rules, valid_tickets)
  possible_fields = Array.new(rules.size) { rules.map { |rule| rule[:field] } }
  valid_tickets.each do |ticket|
    ticket.each_with_index do |value, index|
      possible_fields[index].select! do |field|
        rule = rules.find { |rule| rule[:field] == field }
        rule[:ranges].any? { |(min, max)| value.between?(min, max) }
      end
    end
  end
  until possible_fields.all? { |fields| fields.size == 1 }
    confirmed_fields = possible_fields.select { |fields| fields.size == 1 }.flatten
    possible_fields.each { |fields| fields.reject! { |field| confirmed_fields.include?(field) } if fields.size > 1 }
  end
  possible_fields.flatten
end

def part_two(rules, my_ticket, valid_tickets)
  fields_order = determine_fields(rules, valid_tickets)
  departure_values = fields_order.each_with_index.map { |field, index| my_ticket[index] if field.start_with?("departure") }.compact
  departure_values.reduce(:*)
end

rules, my_ticket, nearby_tickets = parse_input("input.txt")
valid_tickets = nearby_tickets.select { |ticket| valid_ticket?(ticket, rules) }

puts part_one(rules, nearby_tickets) # Output for Part One
puts part_two(rules, my_ticket, valid_tickets) # Output for Part Two
