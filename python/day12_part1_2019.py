
def update_velocity(moons, velocities):
    for i in range(len(moons)):
        for j in range(i + 1, len(moons)):
            for axis in range(3):
                if moons[i][axis] < moons[j][axis]:
                    velocities[i][axis] += 1
                    velocities[j][axis] -= 1
                elif moons[i][axis] > moons[j][axis]:
                    velocities[i][axis] -= 1
                    velocities[j][axis] += 1

def update_positions(moons, velocities):
    for i in range(len(moons)):
        for axis in range(3):
            moons[i][axis] += velocities[i][axis]

def calculate_energy(moons, velocities):
    total_energy = 0
    for i in range(len(moons)):
        potential = sum(abs(x) for x in moons[i])
        kinetic = sum(abs(x) for x in velocities[i])
        total_energy += potential * kinetic
    return total_energy

def main():
    with open('input.txt', 'r') as file:
        moons = [[int(coord.split('=')[1]) for coord in line.strip()[1:-1].split(', ')] for line in file]
    
    velocities = [[0, 0, 0] for _ in range(len(moons))]
    
    for _ in range(1000):
        update_velocity(moons, velocities)
        update_positions(moons, velocities)
    
    print(calculate_energy(moons, velocities))

if __name__ == "__main__":
    main()
