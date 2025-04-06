
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_NODES 2000
#define MAX_NAME_LEN 50
#define HASH_TABLE_SIZE (MAX_NODES * 2)
#define MAX_LINE_LEN (MAX_NAME_LEN * 2 + 5) // name1 + '-' + name2 + '\n' + '\0' + buffer

typedef struct {
    char name[MAX_NAME_LEN];
    int id;
    bool occupied;
} HashEntry;

HashEntry hash_table[HASH_TABLE_SIZE];
char node_names[MAX_NODES][MAX_NAME_LEN];
int node_count = 0;
bool adj[MAX_NODES][MAX_NODES];

unsigned long hash(const char *str) {
    unsigned long hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    return hash;
}

int get_node_id(const char *name) {
    unsigned long h = hash(name) % HASH_TABLE_SIZE;
    unsigned long original_h = h;

    while (hash_table[h].occupied) {
        if (strcmp(hash_table[h].name, name) == 0) {
            return hash_table[h].id;
        }
        h = (h + 1) % HASH_TABLE_SIZE;
        if (h == original_h) {
             fprintf(stderr, "Error: Hash table full.\n");
             exit(EXIT_FAILURE);
        }
    }

    if (node_count >= MAX_NODES) {
        fprintf(stderr, "Error: Maximum number of nodes exceeded (%d).\n", MAX_NODES);
        exit(EXIT_FAILURE);
    }
    strncpy(hash_table[h].name, name, MAX_NAME_LEN - 1);
    hash_table[h].name[MAX_NAME_LEN - 1] = '\0';
    hash_table[h].id = node_count;
    hash_table[h].occupied = true;

    strncpy(node_names[node_count], name, MAX_NAME_LEN - 1);
    node_names[node_count][MAX_NAME_LEN - 1] = '\0';

    return node_count++;
}

int main() {
    memset(hash_table, 0, sizeof(hash_table));
    memset(adj, false, sizeof(adj));

    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    char line[MAX_LINE_LEN];
    char name1[MAX_NAME_LEN];
    char name2[MAX_NAME_LEN];

    while (fgets(line, sizeof(line), f)) {
        line[strcspn(line, "\r\n")] = 0; // Remove trailing newline/CR

        char *hyphen = strchr(line, '-');
        if (!hyphen || hyphen == line || *(hyphen + 1) == '\0') {
             continue; // Skip malformed lines
        }

        *hyphen = '\0';
        strncpy(name1, line, MAX_NAME_LEN - 1);
        name1[MAX_NAME_LEN - 1] = '\0';

        strncpy(name2, hyphen + 1, MAX_NAME_LEN - 1);
        name2[MAX_NAME_LEN - 1] = '\0';

        int id1 = get_node_id(name1);
        int id2 = get_node_id(name2);

        adj[id1][id2] = true;
        adj[id2][id1] = true;
    }
    fclose(f);

    int count = 0;
    for (int i = 0; i < node_count; ++i) {
        for (int j = i + 1; j < node_count; ++j) {
            if (!adj[i][j]) continue;
            for (int k = j + 1; k < node_count; ++k) {
                if (adj[j][k] && adj[k][i]) {
                    if (node_names[i][0] == 't' || node_names[j][0] == 't' || node_names[k][0] == 't') {
                        count++;
                    }
                }
            }
        }
    }

    printf("%d\n", count);

    return EXIT_SUCCESS;
}
