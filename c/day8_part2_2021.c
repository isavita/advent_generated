
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

int compare_chars(const void *a, const void *b) {
    return (*(char*)a - *(char*)b);
}

void sort_string(char *str) {
    qsort(str, strlen(str), sizeof(char), compare_chars);
}

bool contains_all_chars_sorted(const char* larger_sorted, const char* smaller_sorted) {
    while (*smaller_sorted && *larger_sorted) {
        if (*smaller_sorted == *larger_sorted) {
            smaller_sorted++;
            larger_sorted++;
        } else if (*larger_sorted < *smaller_sorted) {
            larger_sorted++;
        } else {
            return false;
        }
    }
    return (*smaller_sorted == '\0');
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        return 1;
    }

    char line[256];
    long long total_sum = 0;

    while (fgets(line, sizeof(line), fp)) {
        char unique_patterns[10][8];
        char output_patterns[4][8];
        char* digit_map[10] = {NULL};
        int lens[10];
        bool assigned[10] = {false};

        int read_count = sscanf(line, "%7s %7s %7s %7s %7s %7s %7s %7s %7s %7s | %7s %7s %7s %7s",
               unique_patterns[0], unique_patterns[1], unique_patterns[2], unique_patterns[3], unique_patterns[4],
               unique_patterns[5], unique_patterns[6], unique_patterns[7], unique_patterns[8], unique_patterns[9],
               output_patterns[0], output_patterns[1], output_patterns[2], output_patterns[3]);

        if (read_count != 14) continue; // Basic check for line format

        for (int i = 0; i < 10; ++i) {
            sort_string(unique_patterns[i]);
            lens[i] = strlen(unique_patterns[i]);
        }
        for (int i = 0; i < 4; ++i) {
            sort_string(output_patterns[i]);
        }

        for(int i = 0; i < 10; ++i) {
            if (lens[i] == 2) { digit_map[1] = unique_patterns[i]; assigned[i] = true; }
            else if (lens[i] == 4) { digit_map[4] = unique_patterns[i]; assigned[i] = true; }
            else if (lens[i] == 3) { digit_map[7] = unique_patterns[i]; assigned[i] = true; }
            else if (lens[i] == 7) { digit_map[8] = unique_patterns[i]; assigned[i] = true; }
        }

        for(int i = 0; i < 10; ++i) {
            if (assigned[i]) continue;
            if (lens[i] == 5 && contains_all_chars_sorted(unique_patterns[i], digit_map[1])) {
                digit_map[3] = unique_patterns[i];
                assigned[i] = true;
            } else if (lens[i] == 6 && contains_all_chars_sorted(unique_patterns[i], digit_map[4])) {
                digit_map[9] = unique_patterns[i];
                assigned[i] = true;
            }
        }

        for(int i = 0; i < 10; ++i) {
            if (assigned[i]) continue;
            if (lens[i] == 6 && contains_all_chars_sorted(unique_patterns[i], digit_map[1])) {
                 digit_map[0] = unique_patterns[i];
                 assigned[i] = true;
                 break;
            }
        }

        for(int i = 0; i < 10; ++i) {
            if (assigned[i]) continue;
            if (lens[i] == 6) {
                 digit_map[6] = unique_patterns[i];
                 assigned[i] = true;
                 break;
            }
        }

        for(int i = 0; i < 10; ++i) {
            if (assigned[i]) continue;
            if (lens[i] == 5) {
                if (contains_all_chars_sorted(digit_map[9], unique_patterns[i])) {
                    digit_map[5] = unique_patterns[i];
                    assigned[i] = true;
                }
            }
        }

        for(int i = 0; i < 10; ++i) {
            if (!assigned[i]) {
                digit_map[2] = unique_patterns[i];
                break;
            }
        }

        int current_number = 0;
        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 10; ++j) {
                 // Check if digit_map[j] is not NULL before comparing
                 if (digit_map[j] != NULL && strcmp(output_patterns[i], digit_map[j]) == 0) {
                    current_number = current_number * 10 + j;
                    break;
                }
            }
        }
        total_sum += current_number;
    }

    fclose(fp);
    printf("%lld\n", total_sum);

    return 0;
}
