def parse_input(filename):
    with open(filename, 'r') as file:
        sections = file.read().split('\n\n')
    
    rules = sections[0].split('\n')
    nearby_tickets = sections[2].split('\n')[1:]  # Skip the "nearby tickets:" line
    
    return rules, nearby_tickets

def get_valid_numbers(rules):
    valid_numbers = set()
    for rule in rules:
        ranges = rule.split(': ')[1].split(' or ')
        for r in ranges:
            start, end = map(int, r.split('-'))
            valid_numbers.update(range(start, end + 1))
    return valid_numbers

def calculate_error_rate(valid_numbers, nearby_tickets):
    error_rate = 0
    for ticket in nearby_tickets:
        for number in map(int, ticket.split(',')):
            if number not in valid_numbers:
                error_rate += number
    return error_rate

def main():
    rules, nearby_tickets = parse_input('input.txt')
    valid_numbers = get_valid_numbers(rules)
    error_rate = calculate_error_rate(valid_numbers, nearby_tickets)
    print(error_rate)

if __name__ == "__main__":
    main()
