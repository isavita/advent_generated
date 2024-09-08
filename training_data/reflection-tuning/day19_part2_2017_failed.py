def follow_path(diagram):
    lines = diagram.splitlines()
    x, y = 0, lines[0].index('|')  # Find starting point
    dx, dy = 1, 0  # Start moving down
    letters = []
    steps = 0

    while True:
        x += dx
        y += dy
        steps += 1

        if x < 0 or x >= len(lines) or y < 0 or y >= len(lines[x]):
            break  # Out of bounds, end of path

        char = lines[x][y]

        if char.isalpha():
            letters.append(char)
        elif char == '+':
            # Change direction
            if dx != 0:  # Moving vertically
                dy = -1 if y > 0 and lines[x][y-1] != ' ' else 1
                dx = 0
            else:  # Moving horizontally
                dx = -1 if x > 0 and lines[x-1][y] != ' ' else 1
                dy = 0
        elif char == ' ':
            break  # End of path

    return ''.join(letters), steps

# Example usage:
diagram = """    |          
    |  +--+    
    A  |  C    
F---|----E|--+ 
    |  |  |  D 
    +B-+  +--+ """

letters, total_steps = follow_path(diagram)
print(f"Letters encountered: {letters}")
print(f"Total steps: {total_steps}")
