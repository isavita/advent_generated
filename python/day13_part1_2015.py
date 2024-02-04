
from itertools import permutations

def calculate_happiness_change(seating, happiness):
    total_change = 0
    for i in range(len(seating)):
        person = seating[i]
        left_neighbor = seating[i-1]
        right_neighbor = seating[(i+1) % len(seating)]
        total_change += happiness[person][left_neighbor] + happiness[person][right_neighbor]
    return total_change

def main():
    happiness = {}
    people = set()

    with open('input.txt', 'r') as file:
        for line in file:
            parts = line.strip().split()
            person = parts[0]
            neighbor = parts[-1][:-1]
            happiness_change = int(parts[3])
            if parts[2] == 'lose':
                happiness_change = -happiness_change
            if person not in happiness:
                happiness[person] = {}
            happiness[person][neighbor] = happiness_change
            people.add(person)

    optimal_happiness = float('-inf')
    for seating in permutations(people):
        total_change = calculate_happiness_change(seating, happiness)
        optimal_happiness = max(optimal_happiness, total_change)

    print(optimal_happiness)

if __name__ == "__main__":
    main()
