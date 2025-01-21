
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LENGTH 256
#define MAX_GROUPS 32
#define MAX_SPRING_LENGTH 128

typedef struct {
    char springs[MAX_SPRING_LENGTH];
    int group[MAX_GROUPS];
    int group_count;
} Row;

typedef struct {
    int springs_idx;
    int group_idx;
    int contiguous_damaged;
} CacheKey;


typedef struct {
    CacheKey key;
    int value;
    struct CacheEntry *next;
} CacheEntry;

#define CACHE_SIZE 100007
CacheEntry *cache[CACHE_SIZE];

unsigned long hash(CacheKey key) {
    unsigned long h = 0;
    h += key.springs_idx * 31;
    h += key.group_idx * 17;
    h += key.contiguous_damaged;
    return h % CACHE_SIZE;
}

int get_cache(CacheKey key) {
    unsigned long h = hash(key);
    CacheEntry *entry = cache[h];
    while (entry) {
        if (entry->key.springs_idx == key.springs_idx &&
            entry->key.group_idx == key.group_idx &&
            entry->key.contiguous_damaged == key.contiguous_damaged) {
            return entry->value;
        }
        entry = entry->next;
    }
    return -1;
}

void put_cache(CacheKey key, int value) {
    unsigned long h = hash(key);
    CacheEntry *new_entry = (CacheEntry *)malloc(sizeof(CacheEntry));
    if (!new_entry) {
      perror("Malloc error");
      exit(EXIT_FAILURE);
    }
    new_entry->key = key;
    new_entry->value = value;
    new_entry->next = cache[h];
    cache[h] = new_entry;
}

void free_cache() {
    for (int i = 0; i < CACHE_SIZE; i++) {
        CacheEntry *entry = cache[i];
        while (entry) {
            CacheEntry *temp = entry;
            entry = entry->next;
            free(temp);
        }
        cache[i] = NULL;
    }
}

void clear_cache() {
    for(int i=0; i<CACHE_SIZE; i++){
        CacheEntry *entry = cache[i];
        while(entry) {
            CacheEntry *temp = entry;
            entry = entry->next;
            free(temp);
        }
        cache[i] = NULL;
    }
}


int countArrangementsRecursive(Row row, int iSprings, int iGroup, int iContiguousDamaged) {
    if (iSprings == strlen(row.springs)) {
        if (iGroup == row.group_count && iContiguousDamaged == 0) {
            return 1;
        } else if (iGroup == row.group_count - 1 && iContiguousDamaged == row.group[iGroup]) {
            return 1;
        }
        return 0;
    }

    CacheKey key = {iSprings, iGroup, iContiguousDamaged};
    int cached_val = get_cache(key);
    if (cached_val != -1) {
        return cached_val;
    }

    int res = 0;
    char c = row.springs[iSprings];
    if (c == '.' || c == '?') {
        if (iContiguousDamaged == 0) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged);
        } else if (iGroup < row.group_count && iContiguousDamaged == row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0);
        }
    }
    if (c == '#' || c == '?') {
        if (iGroup < row.group_count && iContiguousDamaged < row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1);
        }
    }

    put_cache(key, res);
    return res;
}


int countArrangements(Row row) {
    clear_cache();
    return countArrangementsRecursive(row, 0, 0, 0);
}


Row parseLine(char *line) {
    Row row;
    char *token = strtok(line, " ");
    if(token == NULL) {
        fprintf(stderr, "Invalid input line: %s\n", line);
        exit(EXIT_FAILURE);
    }

    strncpy(row.springs, token, MAX_SPRING_LENGTH -1);
    row.springs[MAX_SPRING_LENGTH-1] = '\0';

    token = strtok(NULL, " ");
    if (token == NULL) {
        fprintf(stderr, "Invalid input line: %s\n", line);
        exit(EXIT_FAILURE);
    }
    
    char *num_token = strtok(token, ",");
    int group_idx = 0;
    while (num_token != NULL && group_idx < MAX_GROUPS) {
        row.group[group_idx++] = atoi(num_token);
        num_token = strtok(NULL, ",");
    }
    row.group_count = group_idx;

    return row;
}

Row unfoldRow(Row row, int unfoldingFactor) {
    Row newRow;
    strcpy(newRow.springs, row.springs);
    newRow.group_count = row.group_count;
    memcpy(newRow.group, row.group, sizeof(int) * row.group_count);

    int originalSpringsLen = strlen(row.springs);
    int currentLen = originalSpringsLen;

    for (int i = 1; i < unfoldingFactor; i++) {
        if(currentLen + originalSpringsLen+1 >= MAX_SPRING_LENGTH){
            fprintf(stderr, "Max spring length reached");
            exit(EXIT_FAILURE);
        }
        newRow.springs[currentLen++] = '?';
        strcpy(newRow.springs + currentLen, row.springs);
        currentLen += originalSpringsLen;
        
        for(int j=0; j<row.group_count; j++){
          if(newRow.group_count >= MAX_GROUPS){
            fprintf(stderr, "Max group count reached");
              exit(EXIT_FAILURE);
          }
          newRow.group[newRow.group_count++] = row.group[j];
        }
    }
     newRow.springs[currentLen] = '\0';
    return newRow;
}

int solve() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input file");
        exit(EXIT_FAILURE);
    }

    char line[MAX_LINE_LENGTH];
    int totalArrangements = 0;
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0;
        Row row = parseLine(line);
        totalArrangements += countArrangements(row);
    }

    fclose(file);
    free_cache();
    return totalArrangements;
}


int main() {
    printf("%d\n", solve());
    return 0;
}
