
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define MAX_LINE_LEN 4096
#define INITIAL_PATTERNS_CAPACITY 10

// Function to trim leading/trailing whitespace (including newline) in-place
void trim(char *str) {
    if (!str || *str == '\0') {
        return;
    }
    char *start = str;
    while (isspace((unsigned char)*start)) {
        start++;
    }
    char *end = str + strlen(str) - 1;
    while (end > start && isspace((unsigned char)*end)) {
        end--;
    }
    *(end + 1) = '\0';
    if (start != str) {
        memmove(str, start, end - start + 2); // +1 for char, +1 for null terminator
    }
}

bool can_make(const char *design, char **patterns, int *pattern_lengths, int num_patterns) {
    int n = strlen(design);
    if (n == 0) {
        // Handle empty design string based on problem spec (can it be made from patterns?)
        // Assuming an empty design cannot be made unless there's an empty pattern.
        // Python DP starts dp[0]=True, so empty string is possible if no patterns needed.
        // Let's refine: An empty design *can* be made (base case).
        return true;
    }

    bool *dp = (bool *)malloc((n + 1) * sizeof(bool));
    if (dp == NULL) {
        perror("Failed to allocate memory for DP table");
        exit(EXIT_FAILURE);
    }
    memset(dp, 0, (n + 1) * sizeof(bool)); // Initialize all to false
    dp[0] = true; // Base case: empty prefix can always be formed

    for (int i = 1; i <= n; ++i) {
        for (int j = 0; j < num_patterns; ++j) {
            int lp = pattern_lengths[j];
            // Ensure pattern is not empty if we want to avoid matching empty patterns against non-empty design segments
            // However, the Python logic allows empty patterns if present.
             if (lp == 0 && i > 0) continue; // Skip empty patterns unless i=0 (handled by base case)

            if (i >= lp && dp[i - lp]) {
                // Check if the substring of design matches the pattern
                if (strncmp(design + i - lp, patterns[j], lp) == 0) {
                    dp[i] = true;
                    break; // Found a way to make prefix i, move to next i
                }
            }
        }
    }

    bool result = dp[n];
    free(dp);
    return result;
}

int main() {
    FILE *f = fopen("input.txt", "r");
    if (f == NULL) {
        perror("Error opening input file");
        return EXIT_FAILURE;
    }

    char line_buffer[MAX_LINE_LEN];

    // Read patterns line
    if (fgets(line_buffer, sizeof(line_buffer), f) == NULL) {
        fprintf(stderr, "Error reading patterns line or file empty\n");
        fclose(f);
        return EXIT_FAILURE;
    }
    trim(line_buffer); // Remove potential newline

    int patterns_capacity = INITIAL_PATTERNS_CAPACITY;
    int num_patterns = 0;
    char **patterns = (char **)malloc(patterns_capacity * sizeof(char *));
    int *pattern_lengths = (int *)malloc(patterns_capacity * sizeof(int));

    if (patterns == NULL || pattern_lengths == NULL) {
        perror("Failed to allocate initial memory for patterns");
        fclose(f);
        // Free potentially allocated memory
        free(patterns);
        free(pattern_lengths);
        return EXIT_FAILURE;
    }

    // Split patterns line by comma
    char *token = strtok(line_buffer, ",");
    while (token != NULL) {
        trim(token); // Trim whitespace around the pattern
        int len = strlen(token);
        // Optional: Ignore empty patterns resulting from extra commas like ",," or ", ,"
        // if (len > 0) {
            if (num_patterns >= patterns_capacity) {
                patterns_capacity *= 2;
                char **temp_patterns = (char **)realloc(patterns, patterns_capacity * sizeof(char *));
                int *temp_lengths = (int *)realloc(pattern_lengths, patterns_capacity * sizeof(int));
                if (temp_patterns == NULL || temp_lengths == NULL) {
                    perror("Failed to reallocate memory for patterns");
                    // Cleanup already allocated memory
                    for (int i = 0; i < num_patterns; ++i) free(patterns[i]);
                    free(patterns);
                    free(pattern_lengths);
                    free(temp_patterns); // free potentially allocated temp pointers
                    free(temp_lengths);
                    fclose(f);
                    return EXIT_FAILURE;
                }
                patterns = temp_patterns;
                pattern_lengths = temp_lengths;
            }
            patterns[num_patterns] = strdup(token); // Duplicate string
            if (patterns[num_patterns] == NULL) {
                 perror("Failed to duplicate pattern string");
                 // Cleanup
                 for (int i = 0; i < num_patterns; ++i) free(patterns[i]);
                 free(patterns);
                 free(pattern_lengths);
                 fclose(f);
                 return EXIT_FAILURE;
            }
            pattern_lengths[num_patterns] = len;
            num_patterns++;
        // } // End of optional if (len > 0)
        token = strtok(NULL, ",");
    }

    // Read and discard the second line (separator)
    if (fgets(line_buffer, sizeof(line_buffer), f) == NULL) {
        // Okay if this is the end of the file and there were no designs
    }

    int count = 0;
    // Read designs line by line
    while (fgets(line_buffer, sizeof(line_buffer), f) != NULL) {
        trim(line_buffer); // Remove newline and surrounding whitespace
        if (strlen(line_buffer) == 0) continue; // Skip empty lines if any

        if (can_make(line_buffer, patterns, pattern_lengths, num_patterns)) {
            count++;
        }
    }

    printf("%d\n", count);

    // Cleanup
    for (int i = 0; i < num_patterns; ++i) {
        free(patterns[i]);
    }
    free(patterns);
    free(pattern_lengths);
    fclose(f);

    return EXIT_SUCCESS;
}
