
with open('input.txt', 'r') as file:
    groups = file.read().strip().split('\n\n')

total_count = 0
for group in groups:
    group_answers = group.split('\n')
    group_set = set(''.join(group_answers))
    total_count += len(group_set)

print(total_count)
