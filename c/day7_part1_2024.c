
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h> // For LLONG_MAX if needed, though int should suffice based on Python

#define MAX_LINE_LEN 1024
#define MAX_NUMBERS 64 // Assume max 64 numbers per line

// Evaluates if a target value can be produced by applying + and * operators
bool evaluate_expression(long long target, int numbers[], int count) {
    if (count == 0) {
        return false; // Or handle as error? Python implies at least one number.
    }
    if (count == 1) {
        return (long long)numbers[0] == target;
    }

    int num_operators = count - 1;
    long long limit = 1LL << num_operators; // 2^num_operators

    for (long long i = 0; i < limit; ++i) {
        long long current_result = numbers[0];
        long long temp_i = i; // Use a temporary variable for bit manipulation

        for (int j = 0; j < num_operators; ++j) {
            int op_type = temp_i & 1; // 0 for '+', 1 for '*'
            temp_i >>= 1;

            if (op_type == 0) { // '+'
                current_result += numbers[j + 1];
            } else { // '*'
                 // Check for potential overflow before multiplication if necessary
                /*
                if (numbers[j + 1] != 0 && llabs(current_result) > LLONG_MAX / llabs(numbers[j + 1])) {
                     // Overflow would occur, this path is invalid for standard types
                     // Depending on problem constraints, could break or continue
                     // For simplicity here, assume intermediate results fit in long long
                }
                */
                current_result *= numbers[j + 1];
            }
        }

        if (current_result == target) {
            return true;
        }
    }

    return false;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LEN];
    long long total_sum = 0;
    int numbers[MAX_NUMBERS];

    while (fgets(line, sizeof(line), file) != NULL) {
        // Remove trailing newline if present
        line[strcspn(line, "\n")] = 0;

        char *colon_ptr = strchr(line, ':');
        if (colon_ptr == NULL) {
            // Invalid line format, skip or error
            continue;
        }
        *colon_ptr = '\0'; // Null-terminate the target value part

        long long target_value = atoll(line); // Use atoll for long long
        char *num_str_ptr = colon_ptr + 1;

        // Skip leading spaces after colon
        while (*num_str_ptr == ' ') {
            num_str_ptr++;
        }

        int num_count = 0;
        char *token = strtok(num_str_ptr, " ");
        while (token != NULL && num_count < MAX_NUMBERS) {
            numbers[num_count++] = atoi(token); // Use atoi for int
            token = strtok(NULL, " ");
        }

        if (num_count > 0 && evaluate_expression(target_value, numbers, num_count)) {
            total_sum += target_value;
        }
    }

    fclose(file);
    printf("%lld\n", total_sum);

    return 0;
}
