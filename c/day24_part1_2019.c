
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GRID_SIZE 5
#define MAX_STATES 1000

typedef struct {
    char grid[GRID_SIZE][GRID_SIZE];
} State;

int count_adjacent_bugs(const State *state, int row, int col) {
    int count = 0;
    int dr[] = {-1, 1, 0, 0};
    int dc[] = {0, 0, -1, 1};

    for (int i = 0; i < 4; ++i) {
        int nr = row + dr[i];
        int nc = col + dc[i];

        if (nr >= 0 && nr < GRID_SIZE && nc >= 0 && nc < GRID_SIZE && state->grid[nr][nc] == '#') {
            count++;
        }
    }
    return count;
}

void update_state(State *current, State *next) {
    for (int i = 0; i < GRID_SIZE; ++i) {
        for (int j = 0; j < GRID_SIZE; ++j) {
            int adjacent_bugs = count_adjacent_bugs(current, i, j);
            if (current->grid[i][j] == '#') {
                next->grid[i][j] = (adjacent_bugs == 1) ? '#' : '.';
            } else {
                next->grid[i][j] = (adjacent_bugs == 1 || adjacent_bugs == 2) ? '#' : '.';
            }
        }
    }
}

int calculate_biodiversity(const State *state) {
    int biodiversity = 0;
    int power_of_two = 1;
    for (int i = 0; i < GRID_SIZE; ++i) {
        for (int j = 0; j < GRID_SIZE; ++j) {
            if (state->grid[i][j] == '#') {
                biodiversity += power_of_two;
            }
            power_of_two *= 2;
        }
    }
    return biodiversity;
}

int state_exists(const State *states, int num_states, const State *state) {
    for (int i = 0; i < num_states; ++i) {
        if (memcmp(&states[i], state, sizeof(State)) == 0) {
            return 1;
        }
    }
    return 0;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    State initial_state;
    for (int i = 0; i < GRID_SIZE; ++i) {
        for (int j = 0; j < GRID_SIZE; ++j) {
            initial_state.grid[i][j] = fgetc(file);
        }
        fgetc(file); 
    }
    fclose(file);

    State states[MAX_STATES];
    int num_states = 0;
    states[num_states++] = initial_state;

    State current_state = initial_state;
    State next_state;

    while (1) {
        update_state(&current_state, &next_state);
        if (state_exists(states, num_states, &next_state)) {
            printf("%d\n", calculate_biodiversity(&next_state));
            break;
        }
        states[num_states++] = next_state;
        current_state = next_state;
    }

    return 0;
}
