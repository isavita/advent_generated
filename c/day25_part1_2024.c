
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 256
#define MAX_ITEMS 1000 // Max number of locks or keys
#define BLOCK_HEIGHT 7
#define SHAPE_WIDTH 5
#define SHAPE_HEIGHT 6 // Max height relevant for parsing (rows 0-5 or 1-6)

int locks[MAX_ITEMS][SHAPE_WIDTH];
int keys[MAX_ITEMS][SHAPE_WIDTH];
int num_locks = 0;
int num_keys = 0;

void parse_lock(char b[BLOCK_HEIGHT][MAX_LINE_LEN], int result[SHAPE_WIDTH]) {
    for (int c = 0; c < SHAPE_WIDTH; c++) {
        int cnt = 0;
        // Start from row 1, go down to row 6 (inclusive)
        for (int r = 1; r < BLOCK_HEIGHT; r++) {
            // Check bounds in case line is shorter than SHAPE_WIDTH
            if (b[r][c] == '#') {
                cnt++;
            } else {
                break;
            }
        }
        result[c] = cnt;
    }
}

void parse_key(char b[BLOCK_HEIGHT][MAX_LINE_LEN], int result[SHAPE_WIDTH]) {
    for (int c = 0; c < SHAPE_WIDTH; c++) {
        int cnt = 0;
        // Start from row 5, go up to row 0 (inclusive)
        for (int r = SHAPE_HEIGHT - 1; r >= 0; r--) {
             // Check bounds in case line is shorter than SHAPE_WIDTH
            if (b[r][c] == '#') {
                cnt++;
            } else {
                break;
            }
        }
        result[c] = cnt;
    }
}

bool fits(const int lock[SHAPE_WIDTH], const int key[SHAPE_WIDTH]) {
    for (int i = 0; i < SHAPE_WIDTH; i++) {
        // Python condition is <= 5, max height is 6, so lock+key <= 5 means fits
        if (lock[i] + key[i] > SHAPE_HEIGHT -1 ) {
             return false;
        }
    }
    return true;
}

// Helper to check if a string consists only of '#' up to length n
bool all_hashes(const char *str, int n) {
    for (int i = 0; i < n; i++) {
        if (str[i] != '#') return false;
    }
    // Also check if the string is exactly n characters long before newline/null
    return (str[n] == '\n' || str[n] == '\0');
}

// Helper to get effective length (strip trailing whitespace/newline)
int get_effective_length(const char *str) {
    int len = strlen(str);
    while (len > 0 && (str[len - 1] == '\n' || str[len - 1] == '\r' || str[len - 1] == ' ')) {
        len--;
    }
    return len;
}


int main() {
    FILE *f = fopen("input.txt", "r");
    if (f == NULL) {
        // fprintf(stderr, "Error opening input.txt\n"); // Explanation removed
        printf("0\n"); // Print 0 on error as per python logic likely implied
        return 1;
    }

    char current_block[BLOCK_HEIGHT][MAX_LINE_LEN];
    char line[MAX_LINE_LEN];
    int lines_in_block = 0;
    int total_valid_lines = 0;
    bool block_invalid = false;

    while (fgets(line, sizeof(line), f)) {
        int effective_len = get_effective_length(line);
        if (effective_len == 0) {
            continue; // Skip empty lines like Python's `if line.strip()`
        }
        total_valid_lines++;

        // Copy valid line into block buffer
        strncpy(current_block[lines_in_block], line, MAX_LINE_LEN - 1);
        current_block[lines_in_block][MAX_LINE_LEN - 1] = '\0'; // Ensure null termination

         // Check length constraint early for the current line
        if (effective_len < SHAPE_WIDTH) {
             block_invalid = true; // Mark block as invalid if any line is too short
        }

        lines_in_block++;

        if (lines_in_block == BLOCK_HEIGHT) {
            if (!block_invalid) { // Only process if no line was too short
                bool is_lock = true;
                // Check first line for all '#' up to SHAPE_WIDTH
                for(int i = 0; i < SHAPE_WIDTH; ++i) {
                    if(current_block[0][i] != '#') {
                        is_lock = false;
                        break;
                    }
                }
                 // Optional: check if the line is *exactly* SHAPE_WIDTH hashes long (more strict)
                 // if (get_effective_length(current_block[0]) != SHAPE_WIDTH) is_lock = false;


                if (is_lock) {
                    if (num_locks < MAX_ITEMS) {
                        parse_lock(current_block, locks[num_locks]);
                        num_locks++;
                    } else {
                         // Exceeded storage, handle error - print 0 maybe?
                         // For now, silently ignore like python might if list append failed (unlikely)
                    }
                } else {
                     if (num_keys < MAX_ITEMS) {
                        parse_key(current_block, keys[num_keys]);
                        num_keys++;
                    } else {
                        // Exceeded storage
                    }
                }
            }
            // Reset for next block
            lines_in_block = 0;
            block_invalid = false;
        }
    }
    fclose(f);

    // Check if total lines were a multiple of BLOCK_HEIGHT
    if (total_valid_lines == 0 || total_valid_lines % BLOCK_HEIGHT != 0) {
        printf("0\n");
        return 0;
    }

    int count = 0;
    for (int i = 0; i < num_locks; i++) {
        for (int j = 0; j < num_keys; j++) {
            if (fits(locks[i], keys[j])) {
                count++;
            }
        }
    }

    printf("%d\n", count);

    return 0;
}

