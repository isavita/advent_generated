
import re

def parse_input(input_data):
    rules, my_ticket, nearby_tickets = input_data.split("\n\n")
    
    valid_ranges = {}
    for rule in rules.split("\n"):
        field, ranges = rule.split(": ")
        valid_ranges[field] = []
        for r in ranges.split(" or "):
            start, end = map(int, r.split("-"))
            valid_ranges[field].append(range(start, end+1))
    
    my_ticket = list(map(int, my_ticket.split("\n")[1].split(",")))
    
    nearby_tickets = [list(map(int, ticket.split(","))) for ticket in nearby_tickets.split("\n")[1:]]
    
    return valid_ranges, my_ticket, nearby_tickets

def is_valid(value, ranges):
    return any(value in r for r in ranges)

def ticket_scanning_error_rate(valid_ranges, nearby_tickets):
    error_rate = 0
    for ticket in nearby_tickets:
        for value in ticket:
            if not any(is_valid(value, ranges) for ranges in valid_ranges.values()):
                error_rate += value
    return error_rate

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        input_data = file.read().strip()
    
    valid_ranges, _, nearby_tickets = parse_input(input_data)
    result = ticket_scanning_error_rate(valid_ranges, nearby_tickets)
    print(result)
