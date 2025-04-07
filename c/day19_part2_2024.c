
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LEN 8192
#define INITIAL_PATTERNS_CAP 16

// Function to trim leading/trailing whitespace from a string in-place
char* trim_whitespace(char *str) {
    char *end;
    while(isspace((unsigned char)*str)) str++;
    if(*str == 0) return str; // All spaces?

    end = str + strlen(str) - 1;
    while(end > str && isspace((unsigned char)*end)) end--;
    *(end+1) = 0;

    return str;
}

long long count_ways(const char *design, char **patterns, int *pattern_lengths, int num_patterns) {
    int n = strlen(design);
    long long *dp = calloc(n + 1, sizeof(long long));
    if (!dp) {
        perror("Failed to allocate dp array");
        exit(EXIT_FAILURE);
    }
    dp[0] = 1;

    for (int i = 1; i <= n; ++i) {
        for (int p_idx = 0; p_idx < num_patterns; ++p_idx) {
            int lp = pattern_lengths[p_idx];
            if (lp > 0 && i >= lp) {
                if (strncmp(design + i - lp, patterns[p_idx], lp) == 0) {
                    dp[i] += dp[i - lp];
                }
            }
        }
    }

    long long result = dp[n];
    free(dp);
    return result;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    char line_buffer[MAX_LINE_LEN];
    char **patterns = NULL;
    int *pattern_lengths = NULL;
    int patterns_capacity = 0;
    int num_patterns = 0;

    if (fgets(line_buffer, sizeof(line_buffer), fp) == NULL) {
         fprintf(stderr, "Error reading patterns line or empty file\n");
         fclose(fp);
         return 1;
    }
    line_buffer[strcspn(line_buffer, "\r\n")] = 0;

    char *line_copy = strdup(line_buffer);
     if (!line_copy) {
         perror("Failed to duplicate line buffer");
         fclose(fp);
         return 1;
     }

    char *token = strtok(line_copy, ",");
    while (token) {
        char* trimmed_token = trim_whitespace(token);
        int len = strlen(trimmed_token);

        if (len > 0) {
            if (num_patterns >= patterns_capacity) {
                int new_capacity = patterns_capacity == 0 ? INITIAL_PATTERNS_CAP : patterns_capacity * 2;
                char **temp_p = realloc(patterns, new_capacity * sizeof(char*));
                int *temp_l = realloc(pattern_lengths, new_capacity * sizeof(int));

                if (!temp_p || !temp_l) {
                    perror("Failed to reallocate patterns arrays");
                    free(line_copy);
                    for(int i=0; i<num_patterns; ++i) free(patterns[i]);
                    free(patterns);
                    free(pattern_lengths);
                    fclose(fp);
                    return 1;
                }
                patterns = temp_p;
                pattern_lengths = temp_l;
                patterns_capacity = new_capacity;
            }
            patterns[num_patterns] = strdup(trimmed_token);
            if (!patterns[num_patterns]) {
                 perror("Failed to duplicate pattern token");
                 free(line_copy);
                 for(int i=0; i<num_patterns; ++i) free(patterns[i]);
                 free(patterns);
                 free(pattern_lengths);
                 fclose(fp);
                 return 1;
            }
            pattern_lengths[num_patterns] = len;
            num_patterns++;
        }
        token = strtok(NULL, ",");
    }
    free(line_copy);

    // Read and discard the separator line
    if (fgets(line_buffer, sizeof(line_buffer), fp) == NULL) {
        // Allow empty designs section
    }

    long long total_ways = 0;
    while (fgets(line_buffer, sizeof(line_buffer), fp)) {
        char* design = trim_whitespace(line_buffer);
        if (strlen(design) > 0) {
             total_ways += count_ways(design, patterns, pattern_lengths, num_patterns);
        }
    }

    printf("%lld\n", total_ways);

    fclose(fp);
    for (int i = 0; i < num_patterns; ++i) {
        free(patterns[i]);
    }
    free(patterns);
    free(pattern_lengths);

    return 0;
}
