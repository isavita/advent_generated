
import re

class Rule:
    def __init__(self, name, ranges):
        self.name = name
        self.ranges = ranges
    
    def is_valid(self, value):
        return any(low <= value <= high for low, high in self.ranges)

def to_int(s):
    return int(s)

def parse_ticket(s):
    return [to_int(v) for v in s.split(',')]

def is_valid_ticket(ticket, rules):
    return all(any(rule.is_valid(value) for rule in rules) for value in ticket)

def solve_field_positions(rules, tickets):
    valid_positions = {rule.name: set(range(len(tickets[0]))) for rule in rules}
    for ticket in tickets:
        for idx, value in enumerate(ticket):
            for rule in rules:
                if not rule.is_valid(value):
                    valid_positions[rule.name].discard(idx)
    
    field_positions = {}
    while len(field_positions) < len(rules):
        for name, positions in list(valid_positions.items()):
            if len(positions) == 1:
                pos = positions.pop()
                field_positions[name] = pos
                for other_positions in valid_positions.values():
                    other_positions.discard(pos)
                del valid_positions[name]
    return field_positions

def calculate_departure_product(ticket, field_positions):
    product = 1
    for name, pos in field_positions.items():
        if name.startswith("departure"):
            product *= ticket[pos]
    return product

def main():
    with open("input.txt", "r") as file:
        lines = file.read().splitlines()
    
    rules = []
    my_ticket = []
    nearby_tickets = []
    section = 0
    re_rule = re.compile(r"^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$")

    for line in lines:
        if line == "":
            section += 1
            continue
        if section == 0:
            match = re_rule.match(line)
            if match:
                rules.append(Rule(match.group(1), [(int(match.group(2)), int(match.group(3))), (int(match.group(4)), int(match.group(5)))]))
        elif section == 1:
            if line != "your ticket:":
                my_ticket = parse_ticket(line)
        elif section == 2:
            if line != "nearby tickets:":
                ticket = parse_ticket(line)
                if is_valid_ticket(ticket, rules):
                    nearby_tickets.append(ticket)
    
    field_positions = solve_field_positions(rules, nearby_tickets)
    departure_product = calculate_departure_product(my_ticket, field_positions)

    print(departure_product)

if __name__ == "__main__":
    main()
