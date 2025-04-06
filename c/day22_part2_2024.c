
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_STEPS 2000
#define PATTERN_COUNT 130321 // 19^4
#define MOD (1 << 24)
#define MOD_MASK (MOD - 1)

// Use unsigned int for 's' to ensure well-defined behavior with bitwise ops and modulo.
unsigned int next_secret(unsigned int s) {
    unsigned int x;
    // No need for initial modulo, operations handle it.
    x = s * 64;
    s ^= x;
    s &= MOD_MASK;
    x = s / 32; // Integer division
    s ^= x;
    s &= MOD_MASK;
    x = s * 2048;
    s ^= x;
    s &= MOD_MASK;
    return s;
}

// Calculate the index based on four changes.
// Assumes c1, c2, c3, c4 are already validated to be in [-9, 9].
int encode_change4(int c1, int c2, int c3, int c4) {
    // Pre-calculate powers of 19 for slight potential optimization (compiler might do this anyway)
    const int p1 = 19;
    const int p2 = 19 * 19;
    const int p3 = 19 * 19 * 19;
    return (c1 + 9) + (c2 + 9) * p1 + (c3 + 9) * p2 + (c4 + 9) * p3;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    // --- Read Initials Dynamically ---
    int *initials = NULL;
    int initials_count = 0;
    int initials_capacity = 0;
    int val;
    while (fscanf(fp, "%d", &val) == 1) {
        // Consume potential leftover newline characters after the number
        fscanf(fp, "%*[^\n]");
        fscanf(fp, "%*c");

        if (initials_count >= initials_capacity) {
            initials_capacity = (initials_capacity == 0) ? 16 : initials_capacity * 2;
            int *temp = realloc(initials, initials_capacity * sizeof(int));
            if (!temp) {
                perror("Failed to reallocate memory for initials");
                free(initials);
                fclose(fp);
                return 1;
            }
            initials = temp;
        }
        initials[initials_count++] = val;
    }
    fclose(fp);

    if (initials_count == 0) {
        // Handle empty input file case
         printf("0\n"); // Or appropriate output for no buyers
         return 0;
    }


    // --- Allocate Memory (Optimized: Process one buyer at a time) ---
    // Use long long for global_sum to prevent overflow. Initialize to 0.
    long long *global_sum = calloc(PATTERN_COUNT, sizeof(long long));
    int *prices = malloc((NUM_STEPS + 1) * sizeof(int));
    int *changes = malloc(NUM_STEPS * sizeof(int));
    // Local price buffer: stores the *first* price seen for a pattern *per buyer*. Reset each iteration.
    int *local_price = malloc(PATTERN_COUNT * sizeof(int));

    if (!global_sum || !prices || !changes || !local_price) {
        perror("Failed to allocate memory");
        free(initials);
        free(global_sum);
        free(prices);
        free(changes);
        free(local_price);
        return 1;
    }

    // --- Process Each Buyer ---
    for (int b = 0; b < initials_count; ++b) {
        // Generate prices and changes
        unsigned int s = (unsigned int)initials[b]; // Cast to unsigned for next_secret
        for (int i = 0; i <= NUM_STEPS; ++i) {
            prices[i] = (int)(s % 10); // Store price (0-9)
            s = next_secret(s);
        }
        for (int j = 0; j < NUM_STEPS; ++j) {
            changes[j] = prices[j + 1] - prices[j]; // Calculate change (-9 to 9)
        }

        // Reset local_price buffer efficiently (-1 indicates pattern not seen for this buyer)
        memset(local_price, -1, PATTERN_COUNT * sizeof(int));

        // Process change patterns
        for (int i = 0; i <= NUM_STEPS - 4; ++i) {
            int c1 = changes[i];
            int c2 = changes[i + 1];
            int c3 = changes[i + 2];
            int c4 = changes[i + 3];

            // Check if all changes are within the valid range [-9, 9]
            // Combine checks slightly
            if (abs(c1) <= 9 && abs(c2) <= 9 && abs(c3) <= 9 && abs(c4) <= 9) {
                int idx = encode_change4(c1, c2, c3, c4);
                // If this pattern index hasn't been recorded for *this buyer* yet
                if (local_price[idx] < 0) {
                    local_price[idx] = prices[i + 4]; // Record the price at the end of the pattern
                }
            }
        }

        // Accumulate into global sum
        for (int idx = 0; idx < PATTERN_COUNT; ++idx) {
            if (local_price[idx] >= 0) { // If a price was recorded for this pattern by this buyer
                global_sum[idx] += local_price[idx];
            }
        }
    }

    // --- Find Max Sum ---
    long long max_sum = 0; // Initialize max_sum to 0, assuming sums are non-negative
    for (int i = 0; i < PATTERN_COUNT; ++i) {
        if (global_sum[i] > max_sum) {
            max_sum = global_sum[i];
        }
    }

    printf("%lld\n", max_sum);

    // --- Cleanup ---
    free(initials);
    free(global_sum);
    free(prices);
    free(changes);
    free(local_price);

    return 0;
}
