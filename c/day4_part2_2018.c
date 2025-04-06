
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_LINES 2000
#define MAX_LINE_LEN 100
#define MAX_GUARDS 100 // Assume a maximum number of guards

typedef struct {
    int id;
    int minutes[60];
    int total_sleep;
} Guard;

Guard guards[MAX_GUARDS];
int guard_count = 0;

int find_or_add_guard(int id) {
    for (int i = 0; i < guard_count; ++i) {
        if (guards[i].id == id) {
            return i;
        }
    }
    if (guard_count < MAX_GUARDS) {
        guards[guard_count].id = id;
        guards[guard_count].total_sleep = 0;
        memset(guards[guard_count].minutes, 0, sizeof(guards[guard_count].minutes));
        return guard_count++;
    }
    // Handle error: too many guards (or increase MAX_GUARDS)
    fprintf(stderr, "Error: Exceeded MAX_GUARDS\n");
    exit(1);
}

int compare_lines(const void *a, const void *b) {
    return strcmp(*(const char **)a, *(const char **)b);
}

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return 1;
    }

    char *lines[MAX_LINES];
    char buffer[MAX_LINE_LEN];
    int line_count = 0;

    while (line_count < MAX_LINES && fgets(buffer, sizeof(buffer), f)) {
        // Remove trailing newline if present
        buffer[strcspn(buffer, "\n")] = 0;
        lines[line_count] = malloc(strlen(buffer) + 1);
        if (!lines[line_count]) {
             perror("Malloc failed");
             fclose(f);
             // Free previously allocated lines
             for(int i = 0; i < line_count; ++i) free(lines[i]);
             return 1;
        }
        strcpy(lines[line_count], buffer);
        line_count++;
    }
    fclose(f);

    if (line_count == MAX_LINES && !feof(f)) {
         fprintf(stderr, "Warning: MAX_LINES reached, input might be truncated.\n");
         // Free allocated lines before potentially exiting or continuing
         for(int i = 0; i < line_count; ++i) free(lines[i]);
         // Depending on requirements, might want to exit(1) here
    }


    qsort(lines, line_count, sizeof(char *), compare_lines);

    int current_guard_idx = -1;
    int falls_asleep_minute = -1;

    for (int i = 0; i < line_count; ++i) {
        char *line = lines[i];
        int minute;
        // Extract minute (works for all line types)
        sscanf(line + 15, "%d", &minute); // "[YYYY-MM-DD HH:" -> 15 chars

        if (strstr(line, "#")) {
            int guard_id;
            sscanf(strstr(line, "#") + 1, "%d", &guard_id);
            current_guard_idx = find_or_add_guard(guard_id);
        } else if (strstr(line, "falls asleep")) {
            falls_asleep_minute = minute;
        } else if (strstr(line, "wakes up")) {
            if (current_guard_idx != -1 && falls_asleep_minute != -1) {
                for (int m = falls_asleep_minute; m < minute; ++m) {
                    guards[current_guard_idx].minutes[m]++;
                    guards[current_guard_idx].total_sleep++;
                }
                falls_asleep_minute = -1; // Reset
            }
        }
         free(lines[i]); // Free line memory after processing
    }

    // Part 1
    int max_sleep = -1;
    int sleepiest_guard_idx = -1;
    for (int i = 0; i < guard_count; ++i) {
        if (guards[i].total_sleep > max_sleep) {
            max_sleep = guards[i].total_sleep;
            sleepiest_guard_idx = i;
        }
    }

    int most_common_minute = -1;
    int max_minute_count = -1;
    if (sleepiest_guard_idx != -1) {
        for (int m = 0; m < 60; ++m) {
            if (guards[sleepiest_guard_idx].minutes[m] > max_minute_count) {
                max_minute_count = guards[sleepiest_guard_idx].minutes[m];
                most_common_minute = m;
            }
        }
         printf("%d\n", guards[sleepiest_guard_idx].id * most_common_minute);
    } else {
        printf("0\n"); // Or handle error: No guards found / no sleep recorded
    }


    // Part 2
    int max_freq = -1;
    int max_freq_guard_id = -1;
    int max_freq_minute = -1;

    for (int i = 0; i < guard_count; ++i) {
        for (int m = 0; m < 60; ++m) {
            if (guards[i].minutes[m] > max_freq) {
                max_freq = guards[i].minutes[m];
                max_freq_guard_id = guards[i].id;
                max_freq_minute = m;
            }
        }
    }

     if (max_freq_guard_id != -1) {
        printf("%d\n", max_freq_guard_id * max_freq_minute);
    } else {
         printf("0\n"); // Or handle error
    }

    return 0;
}
