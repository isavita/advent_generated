import re

def parse_vector(s):
    return tuple(map(int, re.findall(r'-?\d+', s)))

def vector_magnitude(v):
    return sum(abs(x) for x in v)

def find_closest_particle(particles):
    min_acc = float('inf')
    candidates = []
    
    for i, (p, v, a) in enumerate(particles):
        acc_mag = vector_magnitude(a)
        if acc_mag < min_acc:
            min_acc = acc_mag
            candidates = [i]
        elif acc_mag == min_acc:
            candidates.append(i)
    
    if len(candidates) == 1:
        return candidates[0]
    
    # Tiebreaker: compare velocities
    min_vel = float('inf')
    vel_candidates = []
    for i in candidates:
        vel_mag = vector_magnitude(particles[i][1])
        if vel_mag < min_vel:
            min_vel = vel_mag
            vel_candidates = [i]
        elif vel_mag == min_vel:
            vel_candidates.append(i)
    
    if len(vel_candidates) == 1:
        return vel_candidates[0]
    
    # Final tiebreaker: compare initial positions
    return min(vel_candidates, key=lambda i: vector_magnitude(particles[i][0]))

def main():
    with open('input.txt', 'r') as f:
        particles = [tuple(parse_vector(part) for part in line.split(', ')) for line in f]
    
    closest_particle = find_closest_particle(particles)
    print(closest_particle)

if __name__ == "__main__":
    main()
