def parse_input(file_path):
    with open(file_path) as f:
        lines = [line.rstrip('\n') for line in f]
    tracks = {}
    carts = []
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char in '^v<>':
                direction = char
                carts.append({'position': (x, y), 'direction': direction, 'next_turn': 'left'})
                tracks[(x, y)] = '|' if direction in 'v^' else '-'
            elif char in '|-+/\\':
                tracks[(x, y)] = char
    return tracks, carts

def move_cart(cart, tracks):
    x, y = cart['position']
    direction = cart['direction']
    if direction == '^': dx, dy = 0, -1
    elif direction == 'v': dx, dy = 0, 1
    elif direction == '<': dx, dy = -1, 0
    elif direction == '>': dx, dy = 1, 0
    new_x, new_y = x + dx, y + dy
    new_pos = (new_x, new_y)
    new_dir = direction
    track = tracks[new_pos]
    if track == '/': new_dir = {'^': '>', 'v': '<', '<': 'v', '>': '^'}[direction]
    elif track == '\\': new_dir = {'^': '<', 'v': '>', '<': '^', '>': 'v'}[direction]
    elif track == '+':
        if cart['next_turn'] == 'left':
            new_dir = {'^': '<', 'v': '>', '<': 'v', '>': '^'}[direction]
            cart['next_turn'] = 'straight'
        elif cart['next_turn'] == 'straight':
            cart['next_turn'] = 'right'
        elif cart['next_turn'] == 'right':
            new_dir = {'^': '>', 'v': '<', '<': '^', '>': 'v'}[direction]
            cart['next_turn'] = 'left'
    cart['position'] = new_pos
    cart['direction'] = new_dir

def simulate(tracks, carts):
    while len(carts) > 1:
        carts.sort(key=lambda c: (c['position'][1], c['position'][0]))
        to_remove = set()
        for i, cart in enumerate(carts):
            if i in to_remove: continue
            move_cart(cart, tracks)
            for j, other_cart in enumerate(carts):
                if i != j and cart['position'] == other_cart['position']:
                    to_remove.update([i, j])
                    break
        carts = [cart for i, cart in enumerate(carts) if i not in to_remove]
    return carts[0]['position']

if __name__ == "__main__":
    tracks, carts = parse_input("input.txt")
    last_cart_position = simulate(tracks, carts)
    print(f"The last cart is at: {last_cart_position[0]},{last_cart_position[1]}")
