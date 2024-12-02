def parse_input(file_path):
    with open(file_path, 'r') as file:
        return [[int(level) for level in line.split()] for line in file]

# check if the report is safe according to the rules:
# the levels are either all increasing or all decreasing.
# any two adjacent levels differ by at least one and at most three.
def is_safe(report):
    if all(report[i] < report[i+1] for i in range(len(report)-1)) or all(report[i] > report[i+1] for i in range(len(report)-1)):
        return all(1 <= abs(report[i] - report[i+1]) <= 3 for i in range(len(report)-1))
    return False

# count the number of safe reports
def count_safe_reports(reports):
    return sum(1 for report in reports if is_safe(report))

print(count_safe_reports(parse_input('input.txt')))
