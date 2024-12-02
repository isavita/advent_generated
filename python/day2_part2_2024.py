def parse_input(file_path):
    with open(file_path, 'r') as file:
        return [[int(level) for level in line.split()] for line in file]

# check if the report is safe according to the rules:
# the levels are either all increasing or all decreasing.
# any two adjacent levels differ by at least one and at most three.
# tolerate a single bad level in what would otherwise be a safe report.
def is_safe(report):
    # First check if it's safe without removing any levels
    if is_safe_sequence(report):
        return True
    
    # If not safe, try removing each level one at a time
    for i in range(len(report)):
        # Create new sequence without the current level
        modified_report = report[:i] + report[i+1:]
        if is_safe_sequence(modified_report):
            return True
    
    return False

def is_safe_sequence(report):
    # Check if sequence is strictly increasing
    is_increasing = all(report[i] < report[i+1] for i in range(len(report)-1))
    # Check if sequence is strictly decreasing
    is_decreasing = all(report[i] > report[i+1] for i in range(len(report)-1))
    
    # If neither increasing nor decreasing, return False
    if not (is_increasing or is_decreasing):
        return False
    
    # Check that all adjacent differences are between 1 and 3
    differences = [abs(report[i] - report[i+1]) for i in range(len(report)-1)]
    return all(1 <= diff <= 3 for diff in differences)

# count the number of safe reports
def count_safe_reports(reports):
    return sum(1 for report in reports if is_safe(report))

print(count_safe_reports(parse_input('input.txt')))
