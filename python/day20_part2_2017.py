
import re

def manhattan_distance(particle):
    return sum(abs(x) for x in particle)

def solve(input_file):
    particles = []
    with open(input_file, 'r') as file:
        for line in file:
            p, v, a = re.findall(r'<(.*?)>', line)
            p = list(map(int, p.split(',')))
            v = list(map(int, v.split(',')))
            a = list(map(int, a.split(',')))
            particles.append([p, v, a])

    for _ in range(1000):
        for particle in particles:
            for i in range(3):
                particle[1][i] += particle[2][i]
                particle[0][i] += particle[1][i]

        positions = {}
        for i, particle in enumerate(particles):
            pos = tuple(particle[0])
            if pos in positions:
                positions[pos].append(i)
            else:
                positions[pos] = [i]

        for pos, indices in positions.items():
            if len(indices) > 1:
                for index in indices:
                    particles[index] = None

        particles = [particle for particle in particles if particle is not None]

    return len(particles)

if __name__ == '__main__':
    print(solve('input.txt'))
