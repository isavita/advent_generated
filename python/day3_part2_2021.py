
def filter_values(values, criteria):
    for i in range(len(values[0])):
        zeros, ones = 0, 0
        for val in values:
            if val[i] == '0':
                zeros += 1
            else:
                ones += 1
        keep = criteria(zeros, ones)
        values = filter_by_bit(values, i, keep)
        if len(values) == 1:
            break
    return values[0]

def filter_by_bit(values, bit_index, keep):
    filtered = []
    for val in values:
        if val[bit_index] == keep:
            filtered.append(val)
    return filtered

values = []
with open('input.txt', 'r') as file:
    for line in file:
        values.append(line.strip())

oxygen_generator_rating = filter_values(values, lambda zeros, ones: '0' if zeros > ones else '1')
oxygen_generator_rating_int = int(oxygen_generator_rating, 2)

co2_scrubber_rating = filter_values(values, lambda zeros, ones: '0' if zeros <= ones else '1')
co2_scrubber_rating_int = int(co2_scrubber_rating, 2)

print(oxygen_generator_rating_int * co2_scrubber_rating_int)
