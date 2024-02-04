
def parse_input(input_str):
    cubes = []
    for line in input_str.split('\n'):
        parts = line.split(' ')
        coords = list(map(int, parts[1].replace('x=', '').replace('y=', '').replace('z=', '').replace('..', ',').split(',')))
        cubes.append({'is_on': parts[0] == 'on', 'x1': coords[0], 'x2': coords[1], 'y1': coords[2], 'y2': coords[3], 'z1': coords[4], 'z2': coords[5]})
    return cubes

def max_int(a, b):
    return max(a, b)

def min_int(a, b):
    return min(a, b)

def solve(input_str):
    cubes = parse_input(input_str)
    final_list = []
    for c in cubes:
        to_add = []
        for final_cube in final_list:
            intersection, did_intersect = get_intersection(final_cube, c)
            if did_intersect:
                to_add.append(intersection)
        if c['is_on']:
            to_add.append(c)
        final_list.extend(to_add)
    total = 0
    for c in final_list:
        total += volume(c)
    return total

def get_intersection(c1, c2):
    x1 = max_int(c1['x1'], c2['x1'])
    x2 = min_int(c1['x2'], c2['x2'])
    y1 = max_int(c1['y1'], c2['y1'])
    y2 = min_int(c1['y2'], c2['y2'])
    z1 = max_int(c1['z1'], c2['z1'])
    z2 = min_int(c1['z2'], c2['z2'])
    if x1 > x2 or y1 > y2 or z1 > z2:
        return {}, False
    intersection_state = c2['is_on'] if c1['is_on'] != c2['is_on'] else not c1['is_on']
    return {'is_on': intersection_state, 'x1': x1, 'x2': x2, 'y1': y1, 'y2': y2, 'z1': z1, 'z2': z2}, True

def volume(c):
    vol = (c['x2'] - c['x1'] + 1) * (c['y2'] - c['y1'] + 1) * (c['z2'] - c['z1'] + 1)
    return vol if c['is_on'] else -vol

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        input_str = file.read().strip()
    result = solve(input_str)
    print(result)
