import re

def read_input(file_path):
    """
    Reads the input file and parses workflows and parts.

    Args:
        file_path (str): Path to the input file.

    Returns:
        workflows (dict): Mapping of workflow names to their list of rules.
        parts (list of dict): List of parts with their ratings.
    """
    workflows = {}
    parts = []
    with open(file_path, 'r') as file:
        lines = file.readlines()

    # Split workflows and parts by the first blank line
    try:
        blank_line_index = lines.index('\n')
    except ValueError:
        # No blank line found
        blank_line_index = len(lines)

    workflow_lines = lines[:blank_line_index]
    part_lines = lines[blank_line_index+1:]

    # Parse workflows
    workflow_pattern = re.compile(r"(\w+)\{([^}]+)\}")
    for line in workflow_lines:
        line = line.strip()
        if not line:
            continue
        match = workflow_pattern.match(line)
        if match:
            workflow_name = match.group(1)
            rules_str = match.group(2)
            rules = []
            for rule in rules_str.split(','):
                rule = rule.strip()
                if ':' in rule:
                    condition, destination = rule.split(':', 1)
                    condition = condition.strip()
                    destination = destination.strip()
                else:
                    # No condition, always applies
                    condition = None
                    destination = rule.strip()
                rules.append((condition, destination))
            workflows[workflow_name] = rules
        else:
            print(f"Invalid workflow line: {line}")

    # Parse parts
    part_pattern = re.compile(r"\{([^}]+)\}")
    for line in part_lines:
        line = line.strip()
        if not line:
            continue
        match = part_pattern.match(line)
        if match:
            ratings_str = match.group(1)
            ratings = {}
            for rating in ratings_str.split(','):
                key, value = rating.split('=')
                key = key.strip()
                value = int(value.strip())
                ratings[key] = value
            parts.append(ratings)
        else:
            print(f"Invalid part line: {line}")

    return workflows, parts

def evaluate_condition(part, condition):
    """
    Evaluates a condition string against a part's ratings.

    Args:
        part (dict): The part's ratings.
        condition (str): The condition string (e.g., "a<2006").

    Returns:
        bool: True if the condition is met, False otherwise.
    """
    if not condition:
        return False
    # Supported operators
    operators = ['>=', '<=', '>', '<', '==', '!=']
    for op in operators:
        if op in condition:
            var, value = condition.split(op)
            var = var.strip()
            value = int(value.strip())
            if var not in part:
                return False
            if op == '>=':
                return part[var] >= value
            elif op == '<=':
                return part[var] <= value
            elif op == '>':
                return part[var] > value
            elif op == '<':
                return part[var] < value
            elif op == '==':
                return part[var] == value
            elif op == '!=':
                return part[var] != value
    # If no operator matches, invalid condition
    print(f"Invalid condition: {condition}")
    return False

def process_part(workflows, part):
    """
    Processes a single part through the workflows to determine acceptance.

    Args:
        workflows (dict): Mapping of workflow names to their list of rules.
        part (dict): The part's ratings.

    Returns:
        bool: True if the part is accepted, False otherwise.
    """
    current_workflow = 'in'
    while True:
        if current_workflow not in workflows:
            print(f"Workflow '{current_workflow}' not found.")
            return False  # Reject if workflow not found

        rules = workflows[current_workflow]
        rule_applied = False
        for condition, destination in rules:
            if condition:
                if evaluate_condition(part, condition):
                    if destination == 'A':
                        return True
                    elif destination == 'R':
                        return False
                    else:
                        current_workflow = destination
                        rule_applied = True
                        break
            else:
                # No condition, always applies
                if destination == 'A':
                    return True
                elif destination == 'R':
                    return False
                else:
                    current_workflow = destination
                    rule_applied = True
                    break
        if not rule_applied:
            # No rule was applied, reject by default
            return False

def main():
    input_file = 'input.txt'
    workflows, parts = read_input(input_file)

    total_heat_loss = 0
    accepted_parts = 0

    for idx, part in enumerate(parts, 1):
        accepted = process_part(workflows, part)
        if accepted:
            accepted_parts += 1
            part_sum = part.get('x', 0) + part.get('m', 0) + part.get('a', 0) + part.get('s', 0)
            total_heat_loss += part_sum
            print(f"Part {idx}: Accepted (Sum: {part_sum})")
        else:
            print(f"Part {idx}: Rejected")

    print(f"\nTotal Accepted Parts: {accepted_parts}")
    print(f"Total Heat Loss: {total_heat_loss}")

if __name__ == "__main__":
    main()
