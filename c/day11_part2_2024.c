
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h> // For uint64_t
#include <ctype.h>  // For isdigit

// --- Configuration ---
#define HASH_MAP_SIZE 16384 // Should be a power of 2 for efficient modulo
#define STEPS 75
#define MAX_LINE_LEN 65536 // Increased buffer size for input line
#define INITIAL_STONES_CAPACITY 1024

// --- Hash Map Implementation ---

typedef struct HashNode {
    char *key;          // Stone string (dynamically allocated)
    uint64_t value;     // Count (use unsigned 64-bit for large counts)
    struct HashNode *next; // For separate chaining
} HashNode;

typedef struct HashMap {
    HashNode **buckets;
    size_t size;
} HashMap;

// Simple DJB2 hash function
unsigned long hash_djb2(const unsigned char *str) {
    unsigned long hash = 5381;
    int c;
    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }
    return hash;
}

HashMap* create_map(size_t size) {
    HashMap *map = malloc(sizeof(HashMap));
    if (!map) return NULL;
    map->size = size;
    map->buckets = calloc(size, sizeof(HashNode*)); // Initialize buckets to NULL
    if (!map->buckets) {
        free(map);
        return NULL;
    }
    return map;
}

// Frees nodes and keys, but keeps the bucket array itself
void clear_map(HashMap *map) {
    if (!map) return;
    for (size_t i = 0; i < map->size; i++) {
        HashNode *node = map->buckets[i];
        while (node) {
            HashNode *temp = node;
            node = node->next;
            free(temp->key);
            free(temp);
        }
        map->buckets[i] = NULL; // Reset bucket pointer
    }
}

// Frees the entire map structure including buckets
void destroy_map(HashMap *map) {
    if (!map) return;
    clear_map(map); // Free nodes and keys first
    free(map->buckets);
    free(map);
}

// Inserts/Updates a key-value pair. Increments value if key exists.
// Takes ownership of the key if a new node is created.
void map_put(HashMap *map, char *key, uint64_t value_increment) {
    unsigned long hash = hash_djb2((unsigned char*)key);
    size_t index = hash & (map->size - 1); // Faster modulo for power-of-2 size

    HashNode *node = map->buckets[index];
    while (node) {
        if (strcmp(node->key, key) == 0) {
            node->value += value_increment;
            free(key); // Free the incoming key as it's a duplicate
            return;
        }
        node = node->next;
    }

    // Key not found, create new node
    HashNode *new_node = malloc(sizeof(HashNode));
    if (!new_node) {
        perror("Failed to allocate hash node");
        free(key); // Avoid leak if node allocation fails
        // Optionally handle error more gracefully (e.g., exit)
        return;
    }
    new_node->key = key; // Takes ownership of the key
    new_node->value = value_increment;
    new_node->next = map->buckets[index];
    map->buckets[index] = new_node;
}

// --- String Manipulation Helpers ---

// Creates a new string, trimming leading zeros. Returns "0" if input is all zeros or empty.
// Caller must free the returned string.
char* trim_leading_zeros(const char *s) {
    size_t len = strlen(s);
    size_t i = 0;
    while (i < len - 1 && s[i] == '0') {
        i++;
    }
    // Now s[i] is the first non-zero char or the last char if all zeros.
    size_t new_len = len - i;
    char *trimmed = malloc(new_len + 1);
    if (!trimmed) return NULL;
    memcpy(trimmed, s + i, new_len);
    trimmed[new_len] = '\0';
    return trimmed;
}

// Splits string s, trims halves. Allocates memory for left and right.
// Caller must free *left and *right.
void split_stone(const char *s, char **left, char **right) {
    size_t len = strlen(s);
    size_t mid = len / 2;

    char *left_temp = malloc(mid + 1);
    char *right_temp = malloc(len - mid + 1);
    if (!left_temp || !right_temp) {
         if(left_temp) free(left_temp);
         if(right_temp) free(right_temp);
         *left = NULL; *right = NULL; // Indicate error
         return;
    }

    strncpy(left_temp, s, mid);
    left_temp[mid] = '\0';
    strcpy(right_temp, s + mid); // strcpy is safe here due to allocated size

    *left = trim_leading_zeros(left_temp);
    *right = trim_leading_zeros(right_temp);

    // Handle cases where trimming resulted in empty strings (should become "0")
    if (*left && (*left)[0] == '\0') { free(*left); *left = strdup("0"); }
    if (*right && (*right)[0] == '\0') { free(*right); *right = strdup("0"); }
    // Handle potential allocation failure in trim_leading_zeros or strdup
    if (!(*left)) *left = strdup("0");
    if (!(*right)) *right = strdup("0");


    free(left_temp);
    free(right_temp);
}

// Multiplies string number s by 2024. Returns new allocated string.
// Caller must free the result.
char* multiply_by_2024(const char *s) {
    size_t s_len = strlen(s);
    if (s_len == 0 || (s_len == 1 && s[0] == '0')) {
        return strdup("0");
    }

    int multiplier[] = {2, 0, 2, 4};
    size_t m_len = 4;
    size_t result_len = s_len + m_len; // Max possible length + 1 for carry
    int *result_int = calloc(result_len, sizeof(int));
    if (!result_int) return NULL;

    // Perform multiplication (schoolbook method)
    for (size_t i = 0; i < s_len; ++i) {
        int s_digit = s[s_len - 1 - i] - '0';
        int carry = 0;
        for (size_t j = 0; j < m_len; ++j) {
            int m_digit = multiplier[m_len - 1 - j];
            int product = s_digit * m_digit + result_int[i + j] + carry;
            result_int[i + j] = product % 10;
            carry = product / 10;
        }
        // Propagate carry
        size_t k = i + m_len;
        while(carry > 0) {
            if (k >= result_len) { // Should not happen with proper sizing, but safety check
                 // Reallocation needed (unlikely with small multiplier, indicates potential issue)
                 // For simplicity here, we might just error or truncate, but proper handling requires realloc
                 break;
            }
            int sum = result_int[k] + carry;
            result_int[k] = sum % 10;
            carry = sum / 10;
            k++;
        }
    }

    // Find the start of the actual number (skip leading zeros in result_int)
    size_t start_index = result_len - 1;
    while (start_index > 0 && result_int[start_index] == 0) {
        start_index--;
    }

    // Allocate final string buffer
    size_t final_len = start_index + 1;
    char *result_str = malloc(final_len + 1); // +1 for null terminator
    if (!result_str) {
        free(result_int);
        return NULL;
    }

    // Convert result_int to string
    for (size_t i = 0; i < final_len; ++i) {
        result_str[i] = result_int[final_len - 1 - i] + '0';
    }
    result_str[final_len] = '\0';

    free(result_int);
    return result_str;
}


// --- Main Simulation ---

int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return 1;
    }

    char *line = malloc(MAX_LINE_LEN);
     if (!line) {
        perror("Failed to allocate line buffer");
        fclose(f);
        return 1;
    }

    if (fgets(line, MAX_LINE_LEN, f) == NULL) {
        fprintf(stderr, "Error reading from input.txt or file is empty.\n");
        free(line);
        fclose(f);
        return 1;
    }
    fclose(f);

    // Remove trailing newline if present
    line[strcspn(line, "\n")] = 0;

    // Initialize hash maps (using two maps for efficient step update)
    HashMap *current_map = create_map(HASH_MAP_SIZE);
    HashMap *next_map = create_map(HASH_MAP_SIZE);
    if (!current_map || !next_map) {
        fprintf(stderr, "Failed to create hash maps\n");
        if (current_map) destroy_map(current_map);
        if (next_map) destroy_map(next_map);
        free(line);
        return 1;
    }


    // Parse initial stones
    char *token = strtok(line, " ");
    while (token != NULL) {
        char *key = strdup(token);
        if (!key) {
             perror("Failed to duplicate token");
             // Handle memory allocation failure during parsing
             destroy_map(current_map);
             destroy_map(next_map);
             free(line);
             return 1;
        }
        map_put(current_map, key, 1); // map_put takes ownership or frees key
        token = strtok(NULL, " ");
    }
    free(line); // Free the original line buffer


    // Simulation loop
    for (int step = 0; step < STEPS; ++step) {
        clear_map(next_map); // Clear the next map before populating

        for (size_t i = 0; i < current_map->size; ++i) {
            HashNode *node = current_map->buckets[i];
            while (node) {
                const char *stone = node->key;
                uint64_t count = node->value;
                size_t len = strlen(stone);

                if (len == 1 && stone[0] == '0') {
                    char* new_key = strdup("1");
                    if (new_key) map_put(next_map, new_key, count); else goto cleanup_error;
                } else if (len % 2 == 0) {
                    char *left = NULL, *right = NULL;
                    split_stone(stone, &left, &right);
                    if (left && right) {
                       map_put(next_map, left, count);  // map_put takes ownership or frees
                       map_put(next_map, right, count); // map_put takes ownership or frees
                    } else {
                        if(left) free(left);
                        if(right) free(right);
                        goto cleanup_error; // Handle allocation failure in split
                    }
                } else {
                    char *new_stone = multiply_by_2024(stone);
                    if (new_stone) {
                       map_put(next_map, new_stone, count); // map_put takes ownership or frees
                    } else {
                        goto cleanup_error; // Handle allocation failure in multiply
                    }
                }
                node = node->next;
            }
        }

        // Swap maps for the next iteration
        HashMap *temp_map = current_map;
        current_map = next_map;
        next_map = temp_map;
    }

    // Calculate total stones
    uint64_t total_stones = 0;
    for (size_t i = 0; i < current_map->size; ++i) {
        HashNode *node = current_map->buckets[i];
        while (node) {
            total_stones += node->value;
            node = node->next;
        }
    }

    // Print result
    printf("%llu\n", (unsigned long long)total_stones); // Use %llu for uint64_t

cleanup_error: // Label for jumping on allocation errors during simulation
    // Cleanup
    destroy_map(current_map);
    destroy_map(next_map); // next_map might be the original current_map after swap

    return 0; // Return 0 on success, maybe return non-zero on error jump
}

