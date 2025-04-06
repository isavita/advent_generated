
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

long long evaluate(char *expression);

long long evaluate_simple(const char *expr) {
    long long current_val = 0;
    long long next_val;
    char op;
    const char *p = expr;
    int n = 0;

    if (sscanf(p, "%lld %n", &current_val, &n) != 1) {
         // Handle cases where the expression might start directly with calculation
         // or is just a single number after parenthesis reduction.
         // If sscanf fails, it might be just a number string.
         char *endptr;
         current_val = strtoll(p, &endptr, 10);
         if (endptr == p) return 0; // Truly empty or invalid
         p = endptr; // Assume number consumed
    } else {
         p += n;
    }


    while (sscanf(p, " %c %lld %n", &op, &next_val, &n) == 2) {
        if (op == '+') {
            current_val += next_val;
        } else if (op == '*') {
            current_val *= next_val;
        }
        p += n;
    }
    return current_val;
}

long long evaluate(char *expression) {
    char buffer[2048]; // Increased buffer size
    strncpy(buffer, expression, sizeof(buffer) - 1);
    buffer[sizeof(buffer) - 1] = '\0';

    char *open_paren;
    while ((open_paren = strrchr(buffer, '(')) != NULL) {
        char *close_paren = strchr(open_paren + 1, ')');
        if (!close_paren) {
             // Should not happen with valid input matching python logic
             return 0; // Error
        }

        *close_paren = '\0'; // Temporarily terminate inner expression
        long long inner_result = evaluate(open_paren + 1); // Recursive call


        // Rebuild string in-place is tricky, use a temporary buffer
        char temp_build[2048];
        *open_paren = '\0'; // Terminate the part before '('

        snprintf(temp_build, sizeof(temp_build), "%s%lld%s",
                 buffer,         // Part before '('
                 inner_result,
                 close_paren + 1); // Part after original ')'

        strncpy(buffer, temp_build, sizeof(buffer) - 1);
        buffer[sizeof(buffer) - 1] = '\0';
    }

    return evaluate_simple(buffer);
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening file");
        return 1;
    }

    long long total_sum = 0;
    char line[2048]; // Increased buffer size

    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) > 0) {
             total_sum += evaluate(line);
        }
    }

    fclose(fp);
    printf("%lld\n", total_sum);
    return 0;
}

