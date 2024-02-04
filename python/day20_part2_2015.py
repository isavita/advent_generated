
import sys

def main():
    with open("input.txt", "r") as file:
        data = file.read().strip()
    
    target = int(data) // 11

    houses = [0] * (target + 1)

    for elf in range(1, target + 1):
        for house in range(elf, min(elf*50, target) + 1, elf):
            houses[house] += elf

    for house_number, presents in enumerate(houses):
        if presents >= target:
            print(house_number)
            break

if __name__ == "__main__":
    main()
