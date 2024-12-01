with open('input.txt', 'r') as file:
    input = file.read().strip()

# split input into two lists of integers by splitting each line on whitespace
pairs = input.split('\n')
locations_ids1 = []
locations_ids2 = []
for pair in pairs:
    location_id1, location_id2 = pair.split()
    locations_ids1.append(int(location_id1))
    locations_ids2.append(int(location_id2))

# sort the lists and calculate absolute distance between the corresponding elements
locations_ids1.sort()
locations_ids2.sort()
distances = [abs(locations_ids1[i] - locations_ids2[i]) for i in range(len(locations_ids1))]

# print the sum of the distances
print(sum(distances))
