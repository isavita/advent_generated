with open('input.txt') as f:
    orbits = [line.strip().split(')') for line in f]

orbit_map = {orbitee: orbiter for orbiter, orbitee in orbits}

def get_orbit_count(obj):
    if obj not in orbit_map:
        return 0
    return 1 + get_orbit_count(orbit_map[obj])

total_orbits = sum(get_orbit_count(obj) for obj in orbit_map.keys())
print(total_orbits)

def get_path_to_com(obj):
    path = []
    while obj in orbit_map:
        obj = orbit_map[obj]
        path.append(obj)
    return path

path_to_you = get_path_to_com('YOU')
path_to_san = get_path_to_com('SAN')

for i, obj in enumerate(path_to_you):
    if obj in path_to_san:
        common_obj = obj
        break

transfers = path_to_you.index(common_obj) + path_to_san.index(common_obj)
print(transfers)