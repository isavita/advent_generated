
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define HASH_TABLE_SIZE 4096
#define MAX_NAME_LEN 5 // 4 chars + null terminator
#define MAX_LINE_LEN 64

typedef struct Monkey {
    char name[MAX_NAME_LEN];
    long long val;
    bool has_val;
    struct Monkey* left;
    struct Monkey* right;
    char op; // '+', '-', '*', '/', '=' (for ==)
} Monkey;

typedef struct HashNode {
    Monkey* monkey;
    struct HashNode* next;
} HashNode;

HashNode* hashTable[HASH_TABLE_SIZE];

unsigned int hash(const char* name) {
    unsigned int hash_val = 0;
    while (*name) {
        hash_val = (hash_val * 31 + *name) % HASH_TABLE_SIZE;
        name++;
    }
    return hash_val;
}

Monkey* find_or_create_monkey(const char* name) {
    unsigned int index = hash(name);
    HashNode* current = hashTable[index];
    while (current) {
        if (strcmp(current->monkey->name, name) == 0) {
            return current->monkey;
        }
        current = current->next;
    }

    Monkey* new_monkey = (Monkey*)malloc(sizeof(Monkey));
    if (!new_monkey) {
        perror("Failed to allocate memory for monkey");
        exit(EXIT_FAILURE);
    }
    strncpy(new_monkey->name, name, MAX_NAME_LEN);
    new_monkey->name[MAX_NAME_LEN - 1] = '\0';
    new_monkey->has_val = false;
    new_monkey->val = 0;
    new_monkey->left = NULL;
    new_monkey->right = NULL;
    new_monkey->op = '\0';

    HashNode* new_node = (HashNode*)malloc(sizeof(HashNode));
     if (!new_node) {
        perror("Failed to allocate memory for hash node");
        free(new_monkey);
        exit(EXIT_FAILURE);
    }
    new_node->monkey = new_monkey;
    new_node->next = hashTable[index];
    hashTable[index] = new_node;

    return new_monkey;
}

bool solve(Monkey* m, long long* result) {
    if (m->has_val) {
        *result = m->val;
        return true;
    }

    if (m->left != NULL && m->right != NULL) {
        long long left_val, right_val;
        bool l_ok = solve(m->left, &left_val);
        bool r_ok = solve(m->right, &right_val);

        if (l_ok && r_ok) {
            switch (m->op) {
                case '+': *result = left_val + right_val; return true;
                case '-': *result = left_val - right_val; return true;
                case '*': *result = left_val * right_val; return true;
                case '/':
                    if (right_val == 0) return false; // Avoid division by zero
                     // Ensure integer division truncates towards zero like Python
                    *result = left_val / right_val;
                    return true;
                 case '=': // Used for expect's root check, not regular solve
                    *result = (left_val == right_val); return true;
            }
        }
    }
    return false;
}

long long expect(Monkey* m, long long target) {
    if (strcmp(m->name, "humn") == 0) {
        return target;
    }

    long long left_val, right_val;
    bool l_ok = solve(m->left, &left_val);
    bool r_ok = solve(m->right, &right_val);

    if (!l_ok) { // Left branch depends on "humn"
        switch (m->op) {
            case '+': return expect(m->left, target - right_val);
            case '-': return expect(m->left, target + right_val);
            case '*': return expect(m->left, target / right_val); // Assumes integer division possible
            case '/': return expect(m->left, target * right_val);
            case '=': return expect(m->left, right_val); // Root comparison
        }
    }

    if (!r_ok) { // Right branch depends on "humn"
         switch (m->op) {
            case '+': return expect(m->right, target - left_val);
            case '-': return expect(m->right, left_val - target);
            case '*': return expect(m->right, target / left_val); // Assumes integer division possible
            case '/': return expect(m->right, left_val / target); // Assumes integer division possible
            case '=': return expect(m->right, left_val); // Root comparison
        }
    }

    // Should not happen if input is well-formed and humn exists
    fprintf(stderr, "Error: Cannot determine expectation path for %s\n", m->name);
    exit(EXIT_FAILURE);
}


void cleanup() {
     for (int i = 0; i < HASH_TABLE_SIZE; ++i) {
        HashNode* current = hashTable[i];
        while (current) {
            HashNode* next = current->next;
            free(current->monkey);
            free(current);
            current = next;
        }
        hashTable[i] = NULL; // Good practice
    }
}

int main() {
    for(int i=0; i < HASH_TABLE_SIZE; ++i) {
        hashTable[i] = NULL;
    }

    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    char line[MAX_LINE_LEN];
    Monkey *current_monkey, *left_monkey, *right_monkey;
    char name[MAX_NAME_LEN];
    char left_name[MAX_NAME_LEN];
    char right_name[MAX_NAME_LEN];
    char op;
    long long val;

    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline

        if (sscanf(line, "%4[^:]: %lld", name, &val) == 2) {
            current_monkey = find_or_create_monkey(name);
            current_monkey->val = val;
            current_monkey->has_val = true;
        } else if (sscanf(line, "%4[^:]: %4s %c %4s", name, left_name, &op, right_name) == 4) {
            current_monkey = find_or_create_monkey(name);
            left_monkey = find_or_create_monkey(left_name);
            right_monkey = find_or_create_monkey(right_name);

            current_monkey->left = left_monkey;
            current_monkey->right = right_monkey;
            current_monkey->op = op;
            current_monkey->has_val = false;
        } else {
             fprintf(stderr, "Error parsing line: %s\n", line);
             fclose(file);
             cleanup();
             return EXIT_FAILURE;
        }
    }
    fclose(file);

    Monkey* root = find_or_create_monkey("root");
    Monkey* humn = find_or_create_monkey("humn");

    if (!root || !humn) {
         fprintf(stderr, "Error: Could not find root or humn monkey.\n");
         cleanup();
         return EXIT_FAILURE;
    }


    humn->has_val = false;
    root->op = '='; // Change root operation to comparison

    long long result = expect(root, 0); // Target value for root comparison doesn't matter directly

    printf("%lld\n", result);

    cleanup();

    return EXIT_SUCCESS;
}
