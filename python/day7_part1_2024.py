from itertools import product

with open('input.txt', 'r') as file:
    input = file.read().strip()

# create a list of tuples with the test value and the numbers
def parse_input(input):
    test_values = []
    for line in input.split('\n'):
        test_value, numbers = line.split(': ')
        test_values.append((int(test_value), [int(num) for num in numbers.split(' ')]))
    return test_values

test_values = parse_input(input)

# evaluate if a test value can be produced by applying + and * operators between numbers
def evaluate_expression(test_value, numbers):
    operators = ['+', '*']
    num_operators = len(numbers) - 1
    
    for ops in product(operators, repeat=num_operators):
        # Start with the first number
        result = numbers[0]
        # Apply operations left to right
        for i in range(num_operators):
            if ops[i] == '+':
                result += numbers[i + 1]
            else:  # ops[i] == '*'
                result *= numbers[i + 1]
                
        if result == test_value:
            # Build expression string for return value
            expression = str(numbers[0])
            for i in range(num_operators):
                expression += f" {ops[i]} {numbers[i+1]}"
            return expression
    return None

# sum all the test values that can be evaluated
def calculate_sum_of_test_values(test_values):
    sum_of_test_values = 0
    for test_value in test_values:
        if evaluate_expression(test_value[0], test_value[1]):
            sum_of_test_values += test_value[0]
    print(sum_of_test_values)
    return sum_of_test_values

calculate_sum_of_test_values(test_values)
