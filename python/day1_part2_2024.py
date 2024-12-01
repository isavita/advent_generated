with open('input.txt', 'r') as file:
    input = file.read().strip()

# split input into two lists of integers by splitting each line on whitespace
pairs = input.split('\n')
locations_ids = []
locations_similarities = []
for pair in pairs:
    location_id, similarity = pair.split()
    locations_ids.append(int(location_id))
    locations_similarities.append(int(similarity))

# multiply each location_id by its count in similarities and sum
total = 0
for location_id in locations_ids:
    count = locations_similarities.count(location_id)
    total += count * location_id

print(total)
