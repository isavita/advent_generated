from collections import defaultdict, deque
import re

# Parse the input file
def parse_input(filename):
    with open(filename, 'r') as file:
        instructions = file.readlines()
    
    # Create a graph of tasks and their prerequisites
    tasks = defaultdict(list)
    indegree = defaultdict(int)
    for line in instructions:
        match = re.match(r"Step (\w) must be finished before step (\w) can begin.", line)
        if match:
            prereq, step = match.groups()
            tasks[prereq].append(step)
            indegree[step] += 1
            if prereq not in indegree:
                indegree[prereq] = 0
    
    return tasks, indegree

# Calculate the duration for each task
def task_duration(task, base_time=60):
    return base_time + ord(task) - ord('A') + 1

# Main function to calculate the total time to complete all tasks
def total_time_to_complete(filename, worker_count=5, base_time=60):
    tasks, indegree = parse_input(filename)
    available = [task for task, degree in indegree.items() if degree == 0]
    available.sort(reverse=True) # Use as a stack for alphabetical order
    in_progress = {}
    time_elapsed = 0
    
    while available or in_progress:
        # Assign available workers to tasks
        while available and len(in_progress) < worker_count:
            task = available.pop()
            in_progress[task] = task_duration(task, base_time)
        
        # Find task with minimum time left to completion
        min_time_left = min(in_progress.values())
        time_elapsed += min_time_left
        
        # Update tasks in progress and check for completion
        completed_tasks = []
        for task in list(in_progress):
            in_progress[task] -= min_time_left
            if in_progress[task] <= 0:
                completed_tasks.append(task)
                del in_progress[task]
        
        # Update indegrees and available tasks based on completions
        for task in completed_tasks:
            for next_task in tasks[task]:
                indegree[next_task] -= 1
                if indegree[next_task] == 0:
                    available.append(next_task)
            available.sort(reverse=True)
    
    return time_elapsed

# Call the function and print the result
print(total_time_to_complete('input.txt'))
