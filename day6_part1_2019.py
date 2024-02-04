with open("input.txt", "r") as file:
    orbits = [line.strip() for line in file]

orbit_map = {}
for orbit in orbits:
    center, satellite = orbit.split(")")
    orbit_map[satellite] = center

total_orbits = 0
for satellite in orbit_map:
    current = satellite
    while current in orbit_map:
        total_orbits += 1
        current = orbit_map[current]

print(total_orbits)