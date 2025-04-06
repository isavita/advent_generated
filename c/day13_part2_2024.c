
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define OFFSET 10000000000000LL
#define MAX_LINE_LEN 256

// Parses the coordinate part of a line (e.g., " X+5 , Y=10 ")
void parse_coords(const char *line_data, long long *x, long long *y) {
    long long val1 = 0, val2 = 0;
    const char *comma = strchr(line_data, ',');
    if (comma) {
        // Find the start of the number for the first part
        const char *p1 = line_data;
        while (*p1 && !isdigit(*p1) && *p1 != '-' && *p1 != '+') p1++;
        if (*p1) val1 = strtoll(p1, NULL, 10);

        // Find the start of the number for the second part
        const char *p2 = comma + 1;
        while (*p2 && !isdigit(*p2) && *p2 != '-' && *p2 != '+') p2++;
        if (*p2) val2 = strtoll(p2, NULL, 10);
    }
     *x = val1;
     *y = val2;
}

// Solves for one machine
long long solve_machine(long long ax, long long ay, long long bx, long long by, long long px, long long py) {
    long long D = ax * by - ay * bx;
    if (D == 0) {
        return -1;
    }

    long long numA = px * by - py * bx;
    long long numB = py * ax - px * ay; // Note: Python version had -px*ay + py*ax

    // Check for integer solutions
    if ((numA % D != 0) || (numB % D != 0)) {
        return -1;
    }

    long long a = numA / D;
    long long b = numB / D;

    // Check for non-negative solutions
    if (a < 0 || b < 0) {
        return -1;
    }

    return 3 * a + b;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        // If file cannot be opened, behave like no machines were solved.
        printf("0 0\n");
        return 1;
    }

    char line[MAX_LINE_LEN];
    long long ax = 0, ay = 0, bx = 0, by = 0, px = 0, py = 0;
    int reading_machine = 0; // Flag: 1 if currently parsing a machine block

    long long total_cost = 0;
    long long solved_count = 0;

    while (fgets(line, sizeof(line), fp)) {
        // Trim trailing whitespace
        int len = strlen(line);
        while (len > 0 && isspace((unsigned char)line[len - 1])) {
            line[--len] = '\0';
        }
        // Trim leading whitespace
        char *start = line;
        while (*start && isspace((unsigned char)*start)) {
            start++;
        }

        if (strlen(start) == 0) { // Blank line or empty line
            if (reading_machine) {
                // End of a machine block, solve it
                long long current_px = px + OFFSET;
                long long current_py = py + OFFSET;
                long long cost = solve_machine(ax, ay, bx, by, current_px, current_py);
                if (cost >= 0) {
                    solved_count++;
                    total_cost += cost;
                }
                // Reset for next machine
                ax = ay = bx = by = px = py = 0;
                reading_machine = 0;
            }
        } else {
            reading_machine = 1;
            char *data_part = NULL;

            // Check for different line types and parse coordinates
            if ((data_part = strstr(start, "Button A:")) != NULL || (data_part = strstr(start, "A:")) != NULL) {
                 // Find the actual start of data after ':'
                 char* colon = strchr(data_part, ':');
                 if (colon) parse_coords(colon + 1, &ax, &ay);
            } else if ((data_part = strstr(start, "Button B:")) != NULL || (data_part = strstr(start, "B:")) != NULL) {
                 char* colon = strchr(data_part, ':');
                 if (colon) parse_coords(colon + 1, &bx, &by);
            } else if ((data_part = strstr(start, "Prize:")) != NULL || (data_part = strstr(start, "P:")) != NULL) {
                 char* colon = strchr(data_part, ':');
                 if (colon) parse_coords(colon + 1, &px, &py);
            }
        }
    }

    // Process the last machine if the file didn't end with a blank line
    if (reading_machine) {
        long long current_px = px + OFFSET;
        long long current_py = py + OFFSET;
        long long cost = solve_machine(ax, ay, bx, by, current_px, current_py);
        if (cost >= 0) {
            solved_count++;
            total_cost += cost;
        }
    }

    fclose(fp);

    printf("%lld %lld\n", solved_count, total_cost);

    return 0;
}
