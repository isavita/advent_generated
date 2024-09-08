from intcode import IntcodeComputer
from collections import deque

def get_scaffold_map(output):
    return [list(line) for line in ''.join(map(chr, output)).strip().split('\n')]

def find_intersections(scaffold_map):
    intersections = []
    for y in range(1, len(scaffold_map) - 1):
        for x in range(1, len(scaffold_map[y]) - 1):
            if scaffold_map[y][x] == '#' and all(scaffold_map[y+dy][x+dx] == '#' for dx, dy in [(0,1), (0,-1), (1,0), (-1,0)]):
                intersections.append((x, y))
    return intersections

def compress_path(full_path):
    # This is a simplified compression algorithm. A more robust one might be needed.
    main_routine = []
    functions = {'A': '', 'B': '', 'C': ''}
    
    def find_repeating(s, max_len=20):
        for length in range(2, min(len(s)//2, max_len//2)):
            for i in range(len(s) - length*2 + 1):
                substring = ','.join(s[i:i+length])
                if len(substring) <= max_len and s.count(substring) > 1:
                    return substring
        return None

    while full_path:
        for func in 'ABC':
            if not functions[func]:
                repeating = find_repeating(full_path)
                if repeating:
                    functions[func] = repeating
                    main_routine.append(func)
                    full_path = [x for x in full_path if ','.join(x) != repeating]
                    break
            elif ','.join(full_path[:len(functions[func].split(','))]) == functions[func]:
                main_routine.append(func)
                full_path = full_path[len(functions[func].split(',')):]
                break
    
    return ','.join(main_routine), functions['A'], functions['B'], functions['C']

def solve(memory):
    # Part 1
    computer = IntcodeComputer(memory)
    output = computer.run()
    scaffold_map = get_scaffold_map(output)
    intersections = find_intersections(scaffold_map)
    alignment_sum = sum(x * y for x, y in intersections)
    print(f"Part 1 result: {alignment_sum}")

    # Part 2
    memory[0] = 2
    computer = IntcodeComputer(memory)
    
    # Here we should implement logic to find the full path
    # This is a placeholder and should be replaced with actual path finding
    full_path = "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2".split(',')
    
    main_routine, func_a, func_b, func_c = compress_path(full_path)
    
    inputs = [
        main_routine + '\n',
        func_a + '\n',
        func_b + '\n',
        func_c + '\n',
        'n\n'  # Continuous video feed
    ]
    
    for inp in inputs:
        computer.add_input([ord(c) for c in inp])
    
    output = computer.run()
    dust_collected = output[-1]  # The last output should be the amount of dust collected
    
    print(f"Part 2 result: {dust_collected}")

# Assuming the input is stored in a file named 'input.txt'
with open('input.txt', 'r') as file:
    memory = list(map(int, file.read().strip().split(',')))

solve(memory)
