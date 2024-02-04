
def main():
    with open("input.txt", "r") as file:
        lines = file.readlines()

    initialState = ""
    rules = {}
    for line in lines:
        if "initial state" in line:
            initialState = line.split(": ")[1].strip()
        elif "=>" in line:
            parts = line.split(" => ")
            rules[parts[0].strip()] = parts[1].strip()

    state = {i: '#' for i, c in enumerate(initialState) if c == '#'}

    previousPattern = ""
    previousSum = 0
    offset = 0
    for generation in range(50000000000):
        newState = {}
        minPot, maxPot = minMaxKeys(state)
        for i in range(minPot - 2, maxPot + 3):
            pattern = "".join(state.get(j, '.') for j in range(i - 2, i + 3))
            if rules.get(pattern) == '#':
                newState[i] = '#'
        state = newState

        currentPattern, currentSum = statePattern(state)
        if currentPattern == previousPattern:
            offset = currentSum - previousSum
            remainingGenerations = 50000000000 - generation - 1
            finalSum = currentSum + offset * remainingGenerations
            print(finalSum)
            return
        previousPattern = currentPattern
        previousSum = currentSum


def minMaxKeys(m):
    minKey = min(m.keys())
    maxKey = max(m.keys())
    return minKey, maxKey


def statePattern(m):
    minPot, maxPot = minMaxKeys(m)
    pattern = "".join(m.get(i, '.') for i in range(minPot, maxPot + 1))
    sum_ = sum(i for i, v in m.items() if v == '#')
    return pattern, sum_


if __name__ == "__main__":
    main()
