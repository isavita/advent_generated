import sys

def min_max_keys(m):
    first = True
    for k in m:
        if first:
            min_key, max_key = k, k
            first = False
        else:
            if k < min_key:
                min_key = k
            if k > max_key:
                max_key = k
    return min_key, max_key

if __name__ == "__main__":
	input_file = open("input.txt", "r")
	initial_state = ""
	rules = {}

	for line in input_file:
		if "initial state" in line:
			initial_state = line.split(": ")[1].strip()
		elif "=>" in line:
			parts = line.split(" => ")
			rules[parts[0]] = parts[1][0]

	state = {}
	for i, c in enumerate(initial_state):
		if c == '#':
			state[i] = '#'

	for generation in range(20):
		new_state = {}
		min_pot, max_pot = min_max_keys(state)
		for i in range(min_pot-2, max_pot+3):
			pattern = ""
			for j in range(i-2, i+3):
				if state.get(j, '.') == '#':
					pattern += "#"
				else:
					pattern += "."
			if rules[pattern] == '#':
				new_state[i] = '#'
		state = new_state

	summation = sum(k for k in state)

	print(summation)