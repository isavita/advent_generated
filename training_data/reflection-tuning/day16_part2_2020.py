import re
from math import prod

def parse_input(filename):
    with open(filename, 'r') as f:
        rules_raw, your_ticket_raw, nearby_tickets_raw = f.read().split('\n\n')

    rules = {}
    for line in rules_raw.split('\n'):
        field, ranges = line.split(': ')
        rules[field] = [tuple(map(int, r.split('-'))) for r in ranges.split(' or ')]

    your_ticket = list(map(int, your_ticket_raw.split('\n')[1].split(',')))
    nearby_tickets = [list(map(int, line.split(','))) for line in nearby_tickets_raw.split('\n')[1:]]

    return rules, your_ticket, nearby_tickets

def is_valid_for_any_field(value, rules):
    return any(any(low <= value <= high for low, high in ranges) for ranges in rules.values())

def solve(filename):
    rules, your_ticket, nearby_tickets = parse_input(filename)

    # Part 1
    error_rate = sum(value for ticket in nearby_tickets for value in ticket if not is_valid_for_any_field(value, rules))
    print(f"Part 1: {error_rate}")

    # Part 2
    valid_tickets = [ticket for ticket in nearby_tickets if all(is_valid_for_any_field(value, rules) for value in ticket)]

    possible_fields = [set(rules.keys()) for _ in range(len(your_ticket))]
    for ticket in valid_tickets:
        for i, value in enumerate(ticket):
            possible_fields[i] = {field for field in possible_fields[i] if any(low <= value <= high for low, high in rules[field])}

    field_positions = {}
    while len(field_positions) < len(rules):
        for i, fields in enumerate(possible_fields):
            if len(fields) == 1:
                field = fields.pop()
                field_positions[field] = i
                for other_fields in possible_fields:
                    other_fields.discard(field)

    departure_values = [your_ticket[pos] for field, pos in field_positions.items() if field.startswith('departure')]
    result = prod(departure_values)
    print(f"Part 2: {result}")

solve('input.txt')
