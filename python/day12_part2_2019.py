import re
from math import gcd

def parse_input(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    
    positions = []
    for line in lines:
        x, y, z = map(int, re.findall(r'-?\d+', line))
        positions.append([x, y, z])
    
    velocities = [[0, 0, 0] for _ in range(len(positions))]
    
    return positions, velocities

def simulate_motion(positions, velocities, steps):
    for _ in range(steps):
        # Update velocities based on gravity
        for i in range(len(positions)):
            for j in range(i+1, len(positions)):
                for k in range(3):
                    if positions[i][k] < positions[j][k]:
                        velocities[i][k] += 1
                        velocities[j][k] -= 1
                    elif positions[i][k] > positions[j][k]:
                        velocities[i][k] -= 1
                        velocities[j][k] += 1
        
        # Update positions based on velocities
        for i in range(len(positions)):
            for j in range(3):
                positions[i][j] += velocities[i][j]
    
    return positions, velocities

def calculate_energy(positions, velocities):
    total_energy = 0
    for i in range(len(positions)):
        potential_energy = sum(abs(pos) for pos in positions[i])
        kinetic_energy = sum(abs(vel) for vel in velocities[i])
        total_energy += potential_energy * kinetic_energy
    
    return total_energy

def find_cycle_length(positions, velocities):
    cycle_lengths = [0, 0, 0]
    initial_state = [[(pos[i], vel[i]) for pos, vel in zip(positions, velocities)] for i in range(3)]
    
    for i in range(3):
        steps = 0
        while True:
            for j in range(len(positions)):
                for k in range(j+1, len(positions)):
                    if positions[j][i] < positions[k][i]:
                        velocities[j][i] += 1
                        velocities[k][i] -= 1
                    elif positions[j][i] > positions[k][i]:
                        velocities[j][i] -= 1
                        velocities[k][i] += 1
            
            for j in range(len(positions)):
                positions[j][i] += velocities[j][i]
            
            steps += 1
            
            if all((pos[i], vel[i]) == initial_state[i][j] for j, (pos, vel) in enumerate(zip(positions, velocities))):
                cycle_lengths[i] = steps
                break
    
    return lcm(cycle_lengths[0], lcm(cycle_lengths[1], cycle_lengths[2]))

def lcm(a, b):
    return abs(a * b) // gcd(a, b)

def main():
    positions, velocities = parse_input('input.txt')
    
    # Part 1
    positions_part1, velocities_part1 = simulate_motion(positions.copy(), velocities.copy(), 1000)
    total_energy = calculate_energy(positions_part1, velocities_part1)
    print("Part 1 - Total energy after 1000 steps:", total_energy)
    
    # Part 2
    cycle_length = find_cycle_length(positions, velocities)
    print("Part 2 - Number of steps to reach the first repeating state:", cycle_length)

if __name__ == '__main__':
    main()
