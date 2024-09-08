def count_combinations(containers, target, index=0, current_sum=0):
    if current_sum == target:
        return 1
    if current_sum > target or index == len(containers):
        return 0
    
    # Include the current container
    include = count_combinations(containers, target, index + 1, current_sum + containers[index])
    
    # Exclude the current container
    exclude = count_combinations(containers, target, index + 1, current_sum)
    
    return include + exclude

# Read input from file
with open('input.txt', 'r') as file:
    containers = [int(line.strip()) for line in file]

# Calculate and print the result
result = count_combinations(containers, 150)
print(result)
