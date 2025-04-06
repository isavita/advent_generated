
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_LINES 200
#define MAX_WIDTH 200
#define MAX_GALAXIES (MAX_LINES * MAX_WIDTH)
#define EXPANSION_FACTOR 1000000LL

typedef struct {
    int x;
    int y;
} Coord;

Coord galaxies[MAX_GALAXIES];
int num_galaxies = 0;
char line_buffer[MAX_WIDTH + 2];
bool row_has_galaxy[MAX_LINES] = {false};
bool col_has_galaxy[MAX_WIDTH] = {false};
int row_offset[MAX_LINES] = {0};
int col_offset[MAX_WIDTH] = {0};

int64_t abs_diff(int64_t a, int64_t b) {
    return (a > b) ? (a - b) : (b - a);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    int width = 0;
    int height = 0;

    while (height < MAX_LINES && fgets(line_buffer, sizeof(line_buffer), file)) {
        line_buffer[strcspn(line_buffer, "\n")] = 0;
        int current_width = strlen(line_buffer);

        if (current_width == 0) continue;

        if (width == 0) {
            width = current_width;
            if (width > MAX_WIDTH) {
                 fprintf(stderr, "Error: Line width %d exceeds MAX_WIDTH %d\n", width, MAX_WIDTH);
                 fclose(file);
                 return 1;
            }
        } else if (current_width != width) {
             fprintf(stderr, "Error: Inconsistent line width (%d vs %d) at line %d\n", current_width, width, height + 1);
             fclose(file);
             return 1;
        }

        for (int x = 0; x < width; ++x) {
            if (line_buffer[x] == '#') {
                if (num_galaxies >= MAX_GALAXIES) {
                     fprintf(stderr, "Error: Too many galaxies\n");
                     fclose(file);
                     return 1;
                }
                galaxies[num_galaxies].x = x;
                galaxies[num_galaxies].y = height;
                row_has_galaxy[height] = true;
                col_has_galaxy[x] = true;
                num_galaxies++;
            }
        }
        height++;
    }
    fclose(file);

    if (height >= MAX_LINES && !feof(file)) {
        fprintf(stderr, "Error: Number of lines exceeds MAX_LINES %d\n", MAX_LINES);
        return 1;
    }


    int current_offset = 0;
    for (int y = 0; y < height; ++y) {
        row_offset[y] = current_offset;
        if (!row_has_galaxy[y]) {
            current_offset++;
        }
    }

    current_offset = 0;
    for (int x = 0; x < width; ++x) {
        col_offset[x] = current_offset;
         if (!col_has_galaxy[x]) {
            current_offset++;
        }
    }

    int64_t total_distance = 0;
    const int64_t expansion_add = EXPANSION_FACTOR - 1;

    for (int i = 0; i < num_galaxies; ++i) {
        for (int j = i + 1; j < num_galaxies; ++j) {
            Coord g1 = galaxies[i];
            Coord g2 = galaxies[j];

            int64_t x1 = (int64_t)g1.x + (int64_t)col_offset[g1.x] * expansion_add;
            int64_t y1 = (int64_t)g1.y + (int64_t)row_offset[g1.y] * expansion_add;
            int64_t x2 = (int64_t)g2.x + (int64_t)col_offset[g2.x] * expansion_add;
            int64_t y2 = (int64_t)g2.y + (int64_t)row_offset[g2.y] * expansion_add;

            total_distance += abs_diff(x1, x2) + abs_diff(y1, y2);
        }
    }

    printf("%lld\n", total_distance);

    return 0;
}
