
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    char *springs;
    int *group;
    int group_len;
} Row;

Row parse_row(char *line) {
    Row row;
    char *space = strchr(line, ' ');
    *space = '\0';
    row.springs = strdup(line);
    char *group_str = space + 1;
    int group_capacity = 10;
    row.group = malloc(group_capacity * sizeof(int));
    row.group_len = 0;
    char *token = strtok(group_str, ",");
    while (token != NULL) {
        if (row.group_len >= group_capacity) {
            group_capacity *= 2;
            row.group = realloc(row.group, group_capacity * sizeof(int));
        }
        row.group[row.group_len++] = atoi(token);
        token = strtok(NULL, ",");
    }
    return row;
}


Row unfold_row(Row row, int unfolding_factor) {
    Row new_row;
    int springs_len = strlen(row.springs);
    int new_springs_len = springs_len * unfolding_factor + (unfolding_factor - 1);
    new_row.springs = malloc((new_springs_len + 1) * sizeof(char));
    
    int pos = 0;
    for(int i = 0; i < unfolding_factor; i++){
        strcpy(new_row.springs + pos, row.springs);
        pos += springs_len;
        if(i < unfolding_factor - 1){
             new_row.springs[pos] = '?';
             pos++;
        }
    }
    new_row.springs[new_springs_len] = '\0';

    new_row.group_len = row.group_len * unfolding_factor;
    new_row.group = malloc(new_row.group_len * sizeof(int));
    for(int i = 0; i < unfolding_factor; i++){
        memcpy(new_row.group + i * row.group_len, row.group, row.group_len * sizeof(int));
    }
    return new_row;
}

typedef struct {
    int iSprings;
    int iGroup;
    int iContiguousDamaged;
} CacheKey;

typedef struct {
    CacheKey key;
    long long value;
    struct CacheEntry *next;
} CacheEntry;

#define CACHE_SIZE 100000
CacheEntry* cache[CACHE_SIZE];

unsigned long hash_key(CacheKey key) {
    unsigned long hash = 17;
    hash = hash * 31 + key.iSprings;
    hash = hash * 31 + key.iGroup;
    hash = hash * 31 + key.iContiguousDamaged;
    return hash % CACHE_SIZE;
}

long long cache_get(CacheKey key){
    unsigned long hash = hash_key(key);
    CacheEntry* entry = cache[hash];
    while (entry != NULL){
        if(entry->key.iSprings == key.iSprings && entry->key.iGroup == key.iGroup && entry->key.iContiguousDamaged == key.iContiguousDamaged)
            return entry->value;
        entry = entry->next;
    }
    return -1;
}

void cache_put(CacheKey key, long long value){
    unsigned long hash = hash_key(key);
    CacheEntry* entry = malloc(sizeof(CacheEntry));
    entry->key = key;
    entry->value = value;
    entry->next = cache[hash];
    cache[hash] = entry;
}


long long count_arrangements_recursive(Row row, int iSprings, int iGroup, int iContiguousDamaged){
    
    if (iSprings == strlen(row.springs)) {
        if (iGroup == row.group_len && iContiguousDamaged == 0) {
            return 1;
        } else if (iGroup == row.group_len - 1 && iContiguousDamaged == row.group[iGroup]) {
            return 1;
        }
        return 0;
    }

    CacheKey key = {iSprings, iGroup, iContiguousDamaged};
    long long cached_value = cache_get(key);
    if(cached_value != -1)
        return cached_value;

    long long res = 0;
    char c = row.springs[iSprings];
    if (c == '.' || c == '?') {
        if (iContiguousDamaged == 0) {
            res += count_arrangements_recursive(row, iSprings + 1, iGroup, iContiguousDamaged);
        } else if (iGroup < row.group_len && iContiguousDamaged == row.group[iGroup]) {
            res += count_arrangements_recursive(row, iSprings + 1, iGroup + 1, 0);
        }
    }
    if (c == '#' || c == '?') {
        if (iGroup < row.group_len && iContiguousDamaged < row.group[iGroup]) {
            res += count_arrangements_recursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1);
        }
    }

    cache_put(key, res);
    return res;
}

long long count_arrangements(Row row) {
     for (int i = 0; i < CACHE_SIZE; i++) {
        cache[i] = NULL;
    }
    return count_arrangements_recursive(row, 0, 0, 0);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    long long total_arrangements = 0;
    
    while ((read = getline(&line, &len, file)) != -1) {
        if (line[read - 1] == '\n') {
            line[read - 1] = '\0';
        }
        Row row = parse_row(line);
        Row unfolded_row = unfold_row(row, 5);
        total_arrangements += count_arrangements(unfolded_row);
        free(row.springs);
        free(row.group);
        free(unfolded_row.springs);
        free(unfolded_row.group);
    }
    
    printf("%lld\n", total_arrangements);
    
    free(line);
    fclose(file);
    return 0;
}
