
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_LINE_LEN 100
#define INITIAL_CAPACITY 100
#define MAX_GUARDS 1000 // Assuming max 1000 distinct guards

// Structure to hold parsed log entry data
typedef struct {
    int year, month, day, hour, minute;
    char action[MAX_LINE_LEN];
} LogEntry;

// Structure to hold statistics for each guard
typedef struct {
    int id;
    int total_sleep;
    int minutes[60]; // Counts for each minute 0-59
} GuardStats;

// Comparison function for qsort, based on timestamp
int compareLogEntries(const void *a, const void *b) {
    const LogEntry *ea = (const LogEntry *)a;
    const LogEntry *eb = (const LogEntry *)b;
    if (ea->year != eb->year) return ea->year - eb->year;
    if (ea->month != eb->month) return ea->month - eb->month;
    if (ea->day != eb->day) return ea->day - eb->day;
    if (ea->hour != eb->hour) return ea->hour - eb->hour;
    return ea->minute - eb->minute;
}

// Simple map/array to store guard stats
GuardStats guard_data[MAX_GUARDS];
int guard_map[MAX_GUARDS * 2]; // Simple hash map (using linear probing) or just an ID -> index map
int guard_count = 0;

// Find or create index for guard ID in guard_data array
int find_or_add_guard_index(int id) {
    for (int i = 0; i < guard_count; ++i) {
        if (guard_data[i].id == id) {
            return i;
        }
    }
    // Add new guard if not found and space permits
    if (guard_count < MAX_GUARDS) {
        guard_data[guard_count].id = id;
        guard_data[guard_count].total_sleep = 0;
        memset(guard_data[guard_count].minutes, 0, sizeof(guard_data[guard_count].minutes));
        return guard_count++;
    }
    fprintf(stderr, "Error: Maximum number of guards (%d) exceeded.\n", MAX_GUARDS);
    exit(EXIT_FAILURE);
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    LogEntry *entries = NULL;
    int entry_count = 0;
    int entry_capacity = 0;
    char line[MAX_LINE_LEN * 2]; // Allow longer lines temporarily

    while (fgets(line, sizeof(line), fp)) {
        if (entry_count >= entry_capacity) {
            entry_capacity = (entry_capacity == 0) ? INITIAL_CAPACITY : entry_capacity * 2;
            LogEntry *new_entries = realloc(entries, entry_capacity * sizeof(LogEntry));
            if (!new_entries) {
                perror("Memory allocation failed");
                free(entries);
                fclose(fp);
                return EXIT_FAILURE;
            }
            entries = new_entries;
        }

        char *action_ptr = strchr(line, ']');
        if (!action_ptr || *(action_ptr + 1) != ' ') {
             continue; // Skip malformed lines
        }
        *action_ptr = '\0'; // Temporarily terminate timestamp string

        int n = sscanf(line, "[%d-%d-%d %d:%d]",
                       &entries[entry_count].year, &entries[entry_count].month,
                       &entries[entry_count].day, &entries[entry_count].hour,
                       &entries[entry_count].minute);

        if (n != 5) {
             continue; // Skip if timestamp parsing failed
        }

        // Copy action string (skip space after ']')
        strncpy(entries[entry_count].action, action_ptr + 2, MAX_LINE_LEN - 1);
        entries[entry_count].action[MAX_LINE_LEN - 1] = '\0';
        // Remove trailing newline if present
        char *newline = strchr(entries[entry_count].action, '\n');
        if (newline) *newline = '\0';

        entry_count++;
    }
    fclose(fp);

    qsort(entries, entry_count, sizeof(LogEntry), compareLogEntries);

    int current_guard_id = -1;
    int asleep_start_minute = -1;
    int current_guard_index = -1;

    for (int i = 0; i < entry_count; ++i) {
        if (strstr(entries[i].action, "Guard")) {
            if (sscanf(entries[i].action, "Guard #%d begins shift", &current_guard_id) == 1) {
                 current_guard_index = find_or_add_guard_index(current_guard_id);
            }
            asleep_start_minute = -1; // Reset sleep state on shift change
        } else if (strstr(entries[i].action, "falls asleep")) {
            asleep_start_minute = entries[i].minute;
        } else if (strstr(entries[i].action, "wakes up")) {
            if (current_guard_index != -1 && asleep_start_minute != -1) {
                int wake_minute = entries[i].minute;
                for (int m = asleep_start_minute; m < wake_minute; ++m) {
                    guard_data[current_guard_index].total_sleep++;
                    guard_data[current_guard_index].minutes[m]++;
                }
            }
             asleep_start_minute = -1; // Reset sleep state after waking
        }
    }

    int max_sleep = -1;
    int sleepiest_guard_index = -1;
    for (int i = 0; i < guard_count; ++i) {
        if (guard_data[i].total_sleep > max_sleep) {
            max_sleep = guard_data[i].total_sleep;
            sleepiest_guard_index = i;
        }
    }

    int best_minute = -1;
    int max_minute_count = -1;
    if (sleepiest_guard_index != -1) {
        for (int m = 0; m < 60; ++m) {
            if (guard_data[sleepiest_guard_index].minutes[m] > max_minute_count) {
                max_minute_count = guard_data[sleepiest_guard_index].minutes[m];
                best_minute = m;
            }
        }
         printf("%d\n", guard_data[sleepiest_guard_index].id * best_minute);
    } else {
         printf("0\n"); // Or indicate no data/error
    }


    free(entries);
    // No need to free guard_data as it's static/global in this version

    return EXIT_SUCCESS;
}
