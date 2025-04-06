
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

// --- SnailNumber Structure ---
typedef struct SnailNumber {
    int value; // -1 for pairs, >= 0 for regular numbers
    struct SnailNumber *left;
    struct SnailNumber *right;
} SnailNumber;

// --- Forward Declarations ---
SnailNumber* parse_snail_number(char **s);
void reduce(SnailNumber* sn);
bool explode(SnailNumber* sn, int depth, int* add_val_left, int* add_val_right);
bool split_num(SnailNumber* sn);
long long magnitude(SnailNumber* sn);
void add_left(SnailNumber* sn, int value);
void add_right(SnailNumber* sn, int value);
void free_snail_number(SnailNumber* sn); // Optional: for memory management

// --- Helper Functions ---
SnailNumber* create_regular(int value) {
    SnailNumber* sn = (SnailNumber*)malloc(sizeof(SnailNumber));
    if (!sn) { perror("malloc failed"); exit(1); }
    sn->value = value;
    sn->left = NULL;
    sn->right = NULL;
    return sn;
}

SnailNumber* create_pair(SnailNumber* left, SnailNumber* right) {
    SnailNumber* sn = (SnailNumber*)malloc(sizeof(SnailNumber));
     if (!sn) { perror("malloc failed"); exit(1); }
    sn->value = -1; // Indicate it's a pair
    sn->left = left;
    sn->right = right;
    return sn;
}

bool is_regular(SnailNumber* sn) {
    return sn->left == NULL && sn->right == NULL;
}

// --- Parsing ---
SnailNumber* parse_snail_number(char **s) {
    if (**s == '[') {
        (*s)++; // Consume '['
        SnailNumber* left = parse_snail_number(s);
        if (**s != ',') { /* error */ fprintf(stderr, "Parse error: expected ','\n"); exit(1); }
        (*s)++; // Consume ','
        SnailNumber* right = parse_snail_number(s);
        if (**s != ']') { /* error */ fprintf(stderr, "Parse error: expected ']'\n"); exit(1); }
        (*s)++; // Consume ']'
        return create_pair(left, right);
    } else if (isdigit(**s)) {
        int value = 0;
        while (isdigit(**s)) {
            value = value * 10 + (**s - '0');
            (*s)++;
        }
        return create_regular(value);
    } else {
         fprintf(stderr, "Parse error: unexpected character %c\n", **s);
         exit(1);
    }
}

// --- Reduction Logic ---
void add_left(SnailNumber* sn, int value) {
    if (is_regular(sn)) {
        sn->value += value;
    } else {
        add_left(sn->left, value);
    }
}

void add_right(SnailNumber* sn, int value) {
    if (is_regular(sn)) {
        sn->value += value;
    } else {
        add_right(sn->right, value);
    }
}

bool explode(SnailNumber* sn, int depth, int* add_val_left, int* add_val_right) {
    if (is_regular(sn)) {
        return false;
    }

    if (depth == 4) {
        *add_val_left = sn->left->value;
        *add_val_right = sn->right->value;
        free(sn->left); // Free children before replacing
        free(sn->right);
        sn->left = NULL;
        sn->right = NULL;
        sn->value = 0; // Become a regular number 0
        return true;
    }

    int current_add_left = 0;
    int current_add_right = 0;

    if (explode(sn->left, depth + 1, &current_add_left, &current_add_right)) {
        if (current_add_right > 0 && sn->right != NULL) {
            add_left(sn->right, current_add_right);
        }
        *add_val_left = current_add_left;
        *add_val_right = 0; // Right value was handled internally
        return true;
    }

    if (explode(sn->right, depth + 1, &current_add_left, &current_add_right)) {
        if (current_add_left > 0 && sn->left != NULL) {
            add_right(sn->left, current_add_left);
        }
        *add_val_left = 0; // Left value was handled internally
        *add_val_right = current_add_right;
        return true;
    }

    return false;
}

bool split_num(SnailNumber* sn) {
    if (is_regular(sn)) {
        if (sn->value >= 10) {
            int left_val = sn->value / 2;
            int right_val = (sn->value + 1) / 2; // Ceiling division
            sn->left = create_regular(left_val);
            sn->right = create_regular(right_val);
            sn->value = -1; // Mark as pair
            return true;
        }
        return false;
    } else {
        if (split_num(sn->left)) {
            return true;
        }
        return split_num(sn->right);
    }
}

void reduce(SnailNumber* sn) {
    while (true) {
        int add_l = 0, add_r = 0;
        if (explode(sn, 0, &add_l, &add_r)) {
            continue;
        }
        if (split_num(sn)) {
            continue;
        }
        break; // No more actions possible
    }
}

SnailNumber* add(SnailNumber* a, SnailNumber* b) {
    SnailNumber* new_sn = create_pair(a, b);
    reduce(new_sn);
    return new_sn;
}

// --- Magnitude ---
long long magnitude(SnailNumber* sn) {
    if (is_regular(sn)) {
        return (long long)sn->value;
    }
    return 3 * magnitude(sn->left) + 2 * magnitude(sn->right);
}

// --- Optional: Memory Freeing ---
void free_snail_number(SnailNumber* sn) {
    if (sn == NULL) {
        return;
    }
    if (!is_regular(sn)) {
        free_snail_number(sn->left);
        free_snail_number(sn->right);
    }
    free(sn);
}


// --- Main ---
#define MAX_LINES 200
#define MAX_LEN 200

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    SnailNumber* numbers[MAX_LINES];
    int count = 0;
    char line[MAX_LEN];

    while (fgets(line, sizeof(line), file) && count < MAX_LINES) {
        line[strcspn(line, "\r\n")] = 0; // Remove trailing newline
        if (strlen(line) > 0) {
             char *ptr = line;
             numbers[count++] = parse_snail_number(&ptr);
        }
    }
    fclose(file);

    if (count == 0) {
        printf("No snailfish numbers found in the file.\n");
        return 0;
    }

    SnailNumber* result = numbers[0];
    for (int i = 1; i < count; i++) {
        result = add(result, numbers[i]);
         // Note: numbers[i] is now part of the 'result' tree and shouldn't be freed separately yet
    }

    printf("%lld\n", magnitude(result));

    // Optional: Clean up the final result tree
    // free_snail_number(result); // Be careful if intermediate trees were needed elsewhere

    return 0;
}

