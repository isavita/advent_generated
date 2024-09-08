def solve_part2(rules, your_ticket, nearby_tickets)
  valid_tickets = nearby_tickets.select { |ticket| valid_ticket?(ticket, rules) }
  field_mapping = determine_field_mapping(rules, valid_tickets)
  
  departure_fields = field_mapping.select { |field, index| field.start_with?('departure') }
  departure_fields.map { |field, index| your_ticket[index] }.reduce(:*)
end

def determine_field_mapping(rules, tickets)
  field_mapping = {}
  all_fields = (0...rules.length).to_a
  
  while field_mapping.length < rules.length
    rules.each_with_index do |(field, ranges), i|
      next if field_mapping.key?(field)
      
      possible_indices = all_fields - field_mapping.values
      valid_indices = possible_indices.select do |index|
        tickets.all? { |ticket| ranges.any? { |range| range.include?(ticket[index]) } }
      end
      
      if valid_indices.length == 1
        field_mapping[field] = valid_indices.first
      end
    end
  end
  
  field_mapping
end

def valid_ticket?(ticket, rules)
  ticket.all? do |value|
    rules.any? { |_, ranges| ranges.any? { |range| range.include?(value) } }
  end
end

# Assume these methods are defined elsewhere
def parse_input(input)
  # Parse the input and return rules, your_ticket, and nearby_tickets
end

input = File.read('input.txt')
rules, your_ticket, nearby_tickets = parse_input(input)

puts solve_part2(rules, your_ticket, nearby_tickets)
