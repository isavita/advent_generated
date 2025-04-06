
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h> // For uint64_t

#define MAX_TILES 144 // Max possible tiles based on typical problem constraints (12x12 grid)
#define TILE_DIM 10
#define MAX_LINE_LEN 15

typedef struct {
    int id;
    char data[TILE_DIM][TILE_DIM + 1];
} Tile;

typedef struct {
    char border_str[TILE_DIM + 1];
    int tile_id;
    int original_border_index; // 0: top, 1: bottom, 2: left, 3: right
} BorderEntry;

// Function to reverse a string in place
void reverse_string(char *str) {
    int len = strlen(str);
    for (int i = 0; i < len / 2; i++) {
        char temp = str[i];
        str[i] = str[len - 1 - i];
        str[len - 1 - i] = temp;
    }
}

// Comparison function for qsort (sorts BorderEntry by border_str)
int compare_borders(const void *a, const void *b) {
    return strcmp(((BorderEntry *)a)->border_str, ((BorderEntry *)b)->border_str);
}

// Function to get a specific border (0: top, 1: bottom, 2: left, 3: right)
void get_border(const Tile *tile, int border_index, char *output) {
    if (border_index == 0) { // Top
        strcpy(output, tile->data[0]);
    } else if (border_index == 1) { // Bottom
        strcpy(output, tile->data[TILE_DIM - 1]);
    } else if (border_index == 2) { // Left
        for (int i = 0; i < TILE_DIM; i++) {
            output[i] = tile->data[i][0];
        }
        output[TILE_DIM] = '\0';
    } else { // Right (border_index == 3)
        for (int i = 0; i < TILE_DIM; i++) {
            output[i] = tile->data[i][TILE_DIM - 1];
        }
        output[TILE_DIM] = '\0';
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Tile tiles[MAX_TILES];
    int tile_count = 0;
    char line[MAX_LINE_LEN];
    int current_row = 0;

    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove trailing newline

        if (strstr(line, "Tile")) {
            if (tile_count > 0 || current_row > 0) { // Start of a new tile (or first tile)
                 if (current_row != 0) tile_count++; // Increment if previous tile had data
            }
             sscanf(line, "Tile %d:", &tiles[tile_count].id);
             current_row = 0;
        } else if (strlen(line) > 0) {
            if (current_row < TILE_DIM) {
                 strncpy(tiles[tile_count].data[current_row], line, TILE_DIM);
                 tiles[tile_count].data[current_row][TILE_DIM] = '\0'; // Ensure null termination
                 current_row++;
            }
        } else {
             // Empty line often separates tiles, handled by "Tile" check now
             if(current_row == TILE_DIM) { // Finished reading a tile
                 tile_count++;
                 current_row = 0; // Reset for safety, though next "Tile" line handles it
             }
        }
    }
     if (current_row == TILE_DIM) { // Account for the last tile if no empty line after it
        tile_count++;
    }
    fclose(file);

    BorderEntry all_borders[MAX_TILES * 8]; // 4 original + 4 reversed per tile
    int border_count = 0;

    for (int i = 0; i < tile_count; i++) {
        for (int b_idx = 0; b_idx < 4; b_idx++) {
            char border[TILE_DIM + 1];
            get_border(&tiles[i], b_idx, border);

            // Add original border
            strcpy(all_borders[border_count].border_str, border);
            all_borders[border_count].tile_id = tiles[i].id;
            all_borders[border_count].original_border_index = b_idx; // Store which original border this is
            border_count++;

            // Add reversed border
            reverse_string(border);
            strcpy(all_borders[border_count].border_str, border);
            all_borders[border_count].tile_id = tiles[i].id;
            all_borders[border_count].original_border_index = b_idx; // Mark it came from the same original
            border_count++;
        }
    }

    // Sort all borders (original and reversed) lexicographically
    qsort(all_borders, border_count, sizeof(BorderEntry), compare_borders);

    int border_match_counts[MAX_TILES * 4] = {0}; // Count matches for each *original* border
                                                   // Indexing: tile_index * 4 + original_border_index

    // Count occurrences of each unique border string
    int i = 0;
    while (i < border_count) {
        int j = i;
        while (j < border_count && strcmp(all_borders[i].border_str, all_borders[j].border_str) == 0) {
            j++;
        }
        int count = j - i; // How many times this specific border string appears

        // Update the match count for the original border(s) this string corresponds to
        for (int k = i; k < j; k++) {
            // Find the tile index corresponding to the tile ID
            int tile_idx = -1;
            for(int t=0; t<tile_count; ++t) {
                if (tiles[t].id == all_borders[k].tile_id) {
                    tile_idx = t;
                    break;
                }
            }
             if (tile_idx != -1) {
                // Use a unique index for each original border side of each tile
                 int border_map_index = tile_idx * 4 + all_borders[k].original_border_index;
                 // We only care if the total count (including reverse) is > 1
                 if (count > 1) {
                     border_match_counts[border_map_index] = count; // Mark as matched
                 } else {
                      border_match_counts[border_map_index] = 1; // Mark as unique edge
                 }
             }
        }
        i = j;
    }


    uint64_t result = 1;
    int corner_count = 0;

    for (int t = 0; t < tile_count; t++) {
        int unique_edges = 0;
        for (int b_idx = 0; b_idx < 4; b_idx++) {
            int border_map_index = t * 4 + b_idx;
            if (border_match_counts[border_map_index] == 1) { // Count == 1 means unique edge
                unique_edges++;
            }
        }
        // A corner tile has exactly 2 unique edges (borders that don't match any other tile)
        if (unique_edges == 2) {
            result *= (uint64_t)tiles[t].id;
            corner_count++;
        }
    }

    // Basic check if we found 4 corners
     if (corner_count != 4) {
        // fprintf(stderr, "Warning: Did not find exactly 4 corner tiles (found %d)\n", corner_count);
     }


    printf("%llu\n", (unsigned long long)result);

    return 0;
}
