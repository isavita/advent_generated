def solve(map_grid, path_instructions):
    # Constants
    CUBE_SIZE = 50
    RIGHT, DOWN, LEFT, UP = 0, 1, 2, 3
    DIRECTIONS = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    # Define face transitions
    face_transitions = {
        (1, 2): (2, 0, lambda r: r, lambda c: 0),
        (1, 3): (3, 0, lambda r: 0, lambda c: c),
        (1, 0): (6, 2, lambda r: CUBE_SIZE - 1 - r, lambda c: CUBE_SIZE - 1),
        (1, 1): (4, 0, lambda r: c, lambda c: 0),
        (2, 0): (5, 2, lambda r: CUBE_SIZE - 1 - r, lambda c: CUBE_SIZE - 1),
        (2, 1): (3, 1, lambda r: 0, lambda c: c),
        (2, 2): (1, 2, lambda r: r, lambda c: CUBE_SIZE - 1),
        (2, 3): (6, 3, lambda r: c, lambda c: CUBE_SIZE - 1),
        (3, 0): (2, 3, lambda r: CUBE_SIZE - 1, lambda c: r),
        (3, 2): (4, 1, lambda r: 0, lambda c: r),
        (3, 3): (1, 1, lambda r: CUBE_SIZE - 1, lambda c: c),
        (4, 0): (5, 0, lambda r: r, lambda c: 0),
        (4, 2): (1, 3, lambda r: c, lambda c: 0),
        (4, 3): (3, 0, lambda r: c, lambda c: 0),
        (5, 0): (2, 2, lambda r: CUBE_SIZE - 1 - r, lambda c: CUBE_SIZE - 1),
        (5, 1): (6, 1, lambda r: 0, lambda c: c),
        (5, 2): (4, 2, lambda r: r, lambda c: CUBE_SIZE - 1),
        (6, 0): (5, 3, lambda r: CUBE_SIZE - 1, lambda c: r),
        (6, 1): (2, 1, lambda r: c, lambda c: 0),
        (6, 2): (1, 0, lambda r: CUBE_SIZE - 1 - r, lambda c: 0),
    }

    # Define starting positions for each face
    face_starts = {
        1: (0, CUBE_SIZE),
        2: (0, 2*CUBE_SIZE),
        3: (CUBE_SIZE, CUBE_SIZE),
        4: (2*CUBE_SIZE, 0),
        5: (2*CUBE_SIZE, CUBE_SIZE),
        6: (3*CUBE_SIZE, 0)
    }

    def get_face(row, col):
        for face, (start_row, start_col) in face_starts.items():
            if start_row <= row < start_row + CUBE_SIZE and start_col <= col < start_col + CUBE_SIZE:
                return face
        return None

    def move_on_cube(row, col, facing, steps, map_grid, face_starts):
        for _ in range(steps):
            new_row, new_col = row + DIRECTIONS[facing][0], col + DIRECTIONS[facing][1]
            new_facing = facing
            current_face = get_face(row, col)
            new_face = get_face(new_row, new_col)

            if new_face != current_face:
                if (current_face, facing) in face_transitions:
                    new_face, new_facing, row_transform, col_transform = face_transitions[(current_face, facing)]
                    face_start_row, face_start_col = face_starts[new_face]
                    local_row, local_col = row % CUBE_SIZE, col % CUBE_SIZE
                    new_local_row, new_local_col = row_transform(local_row), col_transform(local_col)
                    new_row, new_col = face_start_row + new_local_row, face_start_col + new_local_col
                else:
                    break

            if map_grid[new_row][new_col] == '#':
                break
            row, col, facing = new_row, new_col, new_facing
        return row, col, facing

    # Initialize position and facing
    row, col = 0, map_grid[0].index('.')
    facing = RIGHT

    # Process path instructions
    for instruction in path_instructions:
        if instruction.isdigit():
            row, col, facing = move_on_cube(row, col, facing, int(instruction), map_grid, face_starts)
        else:
            facing = (facing + (1 if instruction == 'R' else -1)) % 4

    # Calculate final password
    return 1000 * (row + 1) + 4 * (col + 1) + facing

# The main function and input parsing remain the same
