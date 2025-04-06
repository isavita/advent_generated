
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MASK_LEN 36
#define TABLE_SIZE 16384 // Power of 2 for potentially faster modulo, adjust as needed

typedef struct Node {
    uint64_t key;
    uint64_t value;
    struct Node *next;
} Node;

Node* hashTable[TABLE_SIZE];
char current_mask[MASK_LEN + 1];

// Simple hash function
unsigned int hash_function(uint64_t key) {
    return (unsigned int)(key % TABLE_SIZE);
}

// Insert or update value in hash table
void insert_or_update(uint64_t key, uint64_t value) {
    unsigned int index = hash_function(key);
    Node *current = hashTable[index];
    Node *prev = NULL;

    while (current != NULL) {
        if (current->key == key) {
            current->value = value;
            return;
        }
        prev = current;
        current = current->next;
    }

    // Key not found, create new node
    Node *newNode = (Node *)malloc(sizeof(Node));
    if (!newNode) {
        perror("Failed to allocate memory");
        exit(EXIT_FAILURE);
    }
    newNode->key = key;
    newNode->value = value;
    newNode->next = NULL;

    if (prev == NULL) {
        hashTable[index] = newNode;
    } else {
        prev->next = newNode;
    }
}

// Recursive function to generate addresses and store value
void recursive_store(int bit_index, uint64_t current_address, uint64_t value, const char* mask) {
    if (bit_index < 0) {
        insert_or_update(current_address, value);
        return;
    }

    int mask_pos = MASK_LEN - 1 - bit_index;
    uint64_t bit_mask = 1ULL << bit_index;

    switch (mask[mask_pos]) {
        case '0':
            // No change to address bit (already part of initial address)
            recursive_store(bit_index - 1, current_address, value, mask);
            break;
        case '1':
            // Force bit to 1
            recursive_store(bit_index - 1, current_address | bit_mask, value, mask);
            break;
        case 'X':
            // Floating bit: recurse for both 0 and 1
            recursive_store(bit_index - 1, current_address & ~bit_mask, value, mask); // Force 0
            recursive_store(bit_index - 1, current_address | bit_mask, value, mask);  // Force 1
            break;
    }
}

// Calculate sum of all values in the hash table
uint64_t sum_values() {
    uint64_t total_sum = 0;
    for (int i = 0; i < TABLE_SIZE; ++i) {
        Node *current = hashTable[i];
        while (current != NULL) {
            total_sum += current->value;
            current = current->next;
        }
    }
    return total_sum;
}

// Free allocated memory
void free_memory() {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        Node *current = hashTable[i];
        while (current != NULL) {
            Node *temp = current;
            current = current->next;
            free(temp);
        }
        hashTable[i] = NULL; // Ensure table slot is cleared
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    // Initialize hash table
    for (int i = 0; i < TABLE_SIZE; ++i) {
        hashTable[i] = NULL;
    }

    char line[100];
    uint64_t address, value;

    while (fgets(line, sizeof(line), file)) {
        if (strncmp(line, "mask = ", 7) == 0) {
            strncpy(current_mask, line + 7, MASK_LEN);
            current_mask[MASK_LEN] = '\0'; // Null-terminate
            // Remove trailing newline if present
            char *newline = strchr(current_mask, '\n');
            if (newline) *newline = '\0';

        } else if (sscanf(line, "mem[%llu] = %llu", &address, &value) == 2) {
             recursive_store(MASK_LEN - 1, address, value, current_mask);
        }
    }

    fclose(file);

    uint64_t result = sum_values();
    printf("%llu\n", result);

    free_memory(); // Clean up allocated memory

    return EXIT_SUCCESS;
}
