
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LEN 1024
#define MAX_STACK_SIZE 256

// --- Stack Implementation ---
long long num_stack[MAX_STACK_SIZE];
int num_top = -1;
char op_stack[MAX_STACK_SIZE];
int op_top = -1;

void push_num(long long val) {
    if (num_top < MAX_STACK_SIZE - 1) {
        num_stack[++num_top] = val;
    } else {
        fprintf(stderr, "Error: Number stack overflow\n");
        exit(EXIT_FAILURE);
    }
}

long long pop_num() {
    if (num_top >= 0) {
        return num_stack[num_top--];
    } else {
        fprintf(stderr, "Error: Number stack underflow\n");
        exit(EXIT_FAILURE);
    }
}

void push_op(char op) {
    if (op_top < MAX_STACK_SIZE - 1) {
        op_stack[++op_top] = op;
    } else {
        fprintf(stderr, "Error: Operator stack overflow\n");
        exit(EXIT_FAILURE);
    }
}

char pop_op() {
    if (op_top >= 0) {
        return op_stack[op_top--];
    } else {
        fprintf(stderr, "Error: Operator stack underflow\n");
        exit(EXIT_FAILURE);
    }
}

char peek_op() {
    if (op_top >= 0) {
        return op_stack[op_top];
    }
    return '\0'; // Indicate empty or invalid state
}

// --- Evaluation Logic ---
int precedence(char op, int rule) {
    if (op == '(') return 0; // Parentheses handled separately
    if (rule == 1) { // Simple evaluation (left-to-right)
        return 1; // + and * have same precedence
    } else { // Advanced evaluation (+ before *)
        if (op == '+') return 2;
        if (op == '*') return 1;
    }
    return 0; // Should not happen for valid operators
}

void apply_op() {
    char op = pop_op();
    long long right = pop_num();
    long long left = pop_num();
    long long result = 0;

    if (op == '+') {
        result = left + right;
    } else if (op == '*') {
        result = left * right;
    } else {
         fprintf(stderr, "Error: Unknown operator %c\n", op);
         exit(EXIT_FAILURE);
    }
    push_num(result);
}

long long evaluate(const char *expression, int rule) {
    num_top = -1; // Reset stacks for each evaluation
    op_top = -1;
    const char *p = expression;
    long long num;
    char *endptr;

    while (*p) {
        if (isspace((unsigned char)*p)) {
            p++;
        } else if (isdigit((unsigned char)*p)) {
            num = strtoll(p, &endptr, 10);
            push_num(num);
            p = endptr;
        } else if (*p == '(') {
            push_op('(');
            p++;
        } else if (*p == ')') {
            while (op_top >= 0 && peek_op() != '(') {
                apply_op();
            }
            if (op_top < 0 || peek_op() != '(') {
                 fprintf(stderr, "Error: Mismatched parentheses\n");
                 exit(EXIT_FAILURE);
            }
            pop_op(); // Pop '('
            p++;
        } else if (*p == '+' || *p == '*') {
            while (op_top >= 0 && peek_op() != '(' && precedence(peek_op(), rule) >= precedence(*p, rule)) {
                apply_op();
            }
            push_op(*p);
            p++;
        } else {
             fprintf(stderr, "Error: Invalid character %c\n", *p);
             exit(EXIT_FAILURE);
        }
    }

    while (op_top >= 0) {
         if (peek_op() == '(') {
             fprintf(stderr, "Error: Mismatched parentheses at end\n");
             exit(EXIT_FAILURE);
         }
        apply_op();
    }

    if (num_top != 0) {
         fprintf(stderr, "Error: Invalid final number stack state\n");
         exit(EXIT_FAILURE);
    }
    return pop_num();
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LEN];
    long long total_part1 = 0;
    long long total_part2 = 0;

    while (fgets(line, sizeof(line), file)) {
        // Remove potential newline character
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) > 0) { // Check for empty lines
             total_part1 += evaluate(line, 1); // Rule 1: Simple evaluation
             total_part2 += evaluate(line, 2); // Rule 2: Advanced evaluation
        }
    }

    fclose(file);

    printf("%lld\n", total_part1);
    printf("%lld\n", total_part2);

    return 0;
}
