
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h> // For LLONG_MAX

// Define a structure to hold each test case
typedef struct {
    long long target;
    long long *numbers;
    int num_count;
} TestCase;

// Function to perform the concatenation operation n1 || n2
long long concat_nums(long long n1, long long n2) {
    char s1[30]; // Sufficient for long long
    char s2[30];
    char combined[60];

    snprintf(s1, sizeof(s1), "%lld", n1);
    snprintf(s2, sizeof(s2), "%lld", n2);

    // Basic check to prevent excessive length before combining
    if (strlen(s1) + strlen(s2) >= sizeof(combined)) {
        // This combination would likely result in overflow anyway
        // Return a value indicating failure or impossibility, like LLONG_MAX
        // or handle as an error. For simplicity, we might risk overflow
        // if inputs are assumed "reasonable". A safer approach could check:
        // if (n1 > LLONG_MAX / pow(10, strlen(s2))) { /* handle potential overflow */ }
        // Let's proceed, assuming strtoll will handle overflow detection if needed.
    }


    strcpy(combined, s1);
    strcat(combined, s2);

    char *endptr;
    long long result = strtoll(combined, &endptr, 10);

    // Check if strtoll indicated overflow or invalid conversion
    // This basic check catches some overflows, but not all intermediate ones.
    // if (*endptr != '\0' || result == LLONG_MAX || result == LLONG_MIN) {
       // Handle error or signal impossibility
    // }
    return result;
}


// Evaluates if the target can be reached using the numbers and operators
int evaluate(long long target, long long *numbers, int num_count) {
    if (num_count == 0) return 0;
    if (num_count == 1) return numbers[0] == target;

    int num_ops = num_count - 1;
    int *ops = malloc(num_ops * sizeof(int));
    if (!ops) {
        perror("Failed to allocate memory for operators");
        exit(EXIT_FAILURE);
    }
     // Initialize ops to all 0 (representing '+')
    for (int i = 0; i < num_ops; ++i) ops[i] = 0;

    while (1) {
        long long current_result = numbers[0];
        int possible = 1; // Flag for validity (e.g., if concat overflows)

        for (int i = 0; i < num_ops; ++i) {
            long long next_num = numbers[i + 1];
            switch (ops[i]) {
                case 0: // +
                    // Check potential overflow before addition
                    if ((next_num > 0 && current_result > LLONG_MAX - next_num) ||
                        (next_num < 0 && current_result < LLONG_MIN - next_num)) {
                         possible = 0; break;
                    }
                    current_result += next_num;
                    break;
                case 1: // *
                    // Basic overflow check for multiplication
                    if (next_num != 0 && llabs(current_result) > LLONG_MAX / llabs(next_num)) {
                         possible = 0; break;
                    }
                     // More precise check might be needed depending on signs
                    current_result *= next_num;
                    break;
                case 2: // ||
                    current_result = concat_nums(current_result, next_num);
                    // concat_nums itself doesn't easily signal overflow without more checks
                    // Assume if it returns, it's within long long range for now.
                    break;
            }
             if (!possible) break; // Stop calculation if overflow detected
        }

        if (possible && current_result == target) {
            free(ops);
            return 1;
        }

        // Increment operator combination (treat ops as a base-3 number)
        int k = num_ops - 1;
        while (k >= 0) {
            ops[k]++;
            if (ops[k] < 3) break; // No carry-over needed
            ops[k] = 0;           // Reset and carry-over
            k--;
        }
        if (k < 0) break; // Overflowed, all combinations checked
    }

    free(ops);
    return 0;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    int test_case_capacity = 10;
    int test_case_count = 0;
    TestCase *test_cases = malloc(test_case_capacity * sizeof(TestCase));
     if (!test_cases) { perror("malloc"); fclose(file); return 1; }


    char line[1024];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline

        char *colon_ptr = strchr(line, ':');
        if (!colon_ptr) continue;

        *colon_ptr = '\0';
        long long target = strtoll(line, NULL, 10);
        char *nums_str = colon_ptr + 1;
        while (*nums_str == ' ') nums_str++; // Skip leading spaces

        int num_capacity = 5;
        long long *numbers = malloc(num_capacity * sizeof(long long));
        if (!numbers) { perror("malloc numbers"); /* free prior */ fclose(file); return 1; }
        int num_count = 0;

        char *token = strtok(nums_str, " ");
        while (token != NULL) {
            if (num_count >= num_capacity) {
                num_capacity *= 2;
                long long *temp = realloc(numbers, num_capacity * sizeof(long long));
                if (!temp) { perror("realloc numbers"); free(numbers); /* free prior */ fclose(file); return 1; }
                numbers = temp;
            }
            numbers[num_count++] = strtoll(token, NULL, 10);
            token = strtok(NULL, " ");
        }

         // Optional: Shrink numbers array to exact size
         if (num_count > 0) {
            long long *temp = realloc(numbers, num_count * sizeof(long long));
             if (!temp) { perror("realloc shrink"); free(numbers); /* free prior */ fclose(file); return 1; }
             numbers = temp;
         } else {
             free(numbers); // No numbers parsed, free initial allocation
             numbers = NULL;
         }


        if (test_case_count >= test_case_capacity) {
            test_case_capacity *= 2;
            TestCase *temp = realloc(test_cases, test_case_capacity * sizeof(TestCase));
            if (!temp) { perror("realloc test_cases"); free(numbers); /* free prior */ fclose(file); return 1;}
            test_cases = temp;
        }

        test_cases[test_case_count].target = target;
        test_cases[test_case_count].numbers = numbers;
        test_cases[test_case_count].num_count = num_count;
        test_case_count++;
    }
    fclose(file);

    long long total_sum = 0;
    for (int i = 0; i < test_case_count; ++i) {
        if (evaluate(test_cases[i].target, test_cases[i].numbers, test_cases[i].num_count)) {
            total_sum += test_cases[i].target;
        }
    }

    printf("%lld\n", total_sum);

    // Free allocated memory
    for (int i = 0; i < test_case_count; ++i) {
        free(test_cases[i].numbers); // free(NULL) is safe
    }
    free(test_cases);

    return 0;
}
