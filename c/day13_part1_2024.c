
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

typedef struct {
    int ax, ay;
    int bx, by;
    int px, py;
} Machine;

// Function to trim leading/trailing whitespace and newline
char *trim(char *str) {
    char *end;
    while (isspace((unsigned char)*str)) str++;
    if (*str == 0) return str;
    end = str + strlen(str) - 1;
    while (end > str && isspace((unsigned char)*end)) end--;
    end[1] = '\0';
    // Remove trailing newline specifically if fgets included it
    size_t len = strlen(str);
     if (len > 0 && str[len - 1] == '\n') {
         str[len - 1] = '\0';
     }
    return str;
}


int solve_machine(Machine m) {
    int min_cost = -1;
    for (int a_count = 0; a_count <= 100; ++a_count) {
        for (int b_count = 0; b_count <= 100; ++b_count) {
            int x = m.ax * a_count + m.bx * b_count;
            int y = m.ay * a_count + m.by * b_count;
            if (x == m.px && y == m.py) {
                int cost = a_count * 3 + b_count;
                if (min_cost == -1 || cost < min_cost) {
                    min_cost = cost;
                }
            }
        }
    }
    return min_cost;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char buffer[256];
    Machine current_machine;
    int solved_count = 0;
    long long total_cost = 0;

    int lines_read = 0;
    int found_a = 0, found_b = 0, found_p = 0;

    while (fgets(buffer, sizeof(buffer), fp) != NULL) {
        char* line = trim(buffer);

        if (strlen(line) == 0) { // Empty line signifies end of a machine block
            if (lines_read > 0 && found_a && found_b && found_p) {
                int cost = solve_machine(current_machine);
                if (cost != -1) {
                    solved_count++;
                    total_cost += cost;
                }
            }
            // Reset for the next machine
            lines_read = 0;
            found_a = 0;
            found_b = 0;
            found_p = 0;
        } else {
            lines_read++;
             // Use sscanf carefully, checking return values
            if (sscanf(line, " Button A: X+%d, Y+%d", &current_machine.ax, &current_machine.ay) == 2 ||
                sscanf(line, " A: X+%d, Y+%d", &current_machine.ax, &current_machine.ay) == 2) {
                 found_a = 1;
            } else if (sscanf(line, " Button B: X+%d, Y+%d", &current_machine.bx, &current_machine.by) == 2 ||
                       sscanf(line, " B: X+%d, Y+%d", &current_machine.bx, &current_machine.by) == 2) {
                 found_b = 1;
            } else if (sscanf(line, " Prize: X=%d, Y=%d", &current_machine.px, &current_machine.py) == 2 ||
                       sscanf(line, " P: X=%d, Y=%d", &current_machine.px, &current_machine.py) == 2) {
                 found_p = 1;
            }
        }
    }

    // Process the last machine if the file didn't end with a blank line
    if (lines_read > 0 && found_a && found_b && found_p) {
        int cost = solve_machine(current_machine);
        if (cost != -1) {
            solved_count++;
            total_cost += cost;
        }
    }

    fclose(fp);

    printf("%d %lld\n", solved_count, total_cost);

    return 0;
}
