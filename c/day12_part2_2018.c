
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LENGTH 256
#define MAX_STATE_SIZE 2000

typedef struct {
    int pot;
    char plant;
} Pot;

typedef struct {
    Pot *pots;
    int size;
    int capacity;
} State;


State create_state() {
    State s;
    s.pots = NULL;
    s.size = 0;
    s.capacity = 0;
    return s;
}

void state_add(State *s, int pot, char plant) {
    if(s->size == s->capacity) {
        s->capacity = s->capacity == 0 ? 1 : s->capacity * 2;
        s->pots = (Pot *)realloc(s->pots, sizeof(Pot) * s->capacity);
    }
    s->pots[s->size].pot = pot;
    s->pots[s->size].plant = plant;
    s->size++;
}

void free_state(State *s) {
    free(s->pots);
    s->pots = NULL;
    s->size = 0;
    s->capacity = 0;
}
int compare_pots(const void *a, const void *b) {
    Pot potA = *(const Pot*)a;
    Pot potB = *(const Pot*)b;
    return potA.pot - potB.pot;
}

int find_pot_index(const State *s, int pot) {
    for(int i = 0; i < s->size; i++){
        if(s->pots[i].pot == pot){
            return i;
        }
    }
    return -1;
}


char state_get(const State *s, int pot) {
    int index = find_pot_index(s, pot);
    if (index != -1) {
        return s->pots[index].plant;
    }
    return '.';
}

int min_pot(const State *s) {
    if (s->size == 0) return 0;
    int min = s->pots[0].pot;
     for (int i = 1; i < s->size; i++) {
        if (s->pots[i].pot < min) {
            min = s->pots[i].pot;
        }
    }
    return min;
}

int max_pot(const State *s) {
    if (s->size == 0) return 0;
    int max = s->pots[0].pot;
    for (int i = 1; i < s->size; i++) {
        if (s->pots[i].pot > max) {
            max = s->pots[i].pot;
        }
    }
    return max;
}


long long state_sum(const State *s) {
    long long sum = 0;
    for (int i = 0; i < s->size; i++) {
        if(s->pots[i].plant == '#'){
            sum += s->pots[i].pot;
        }
    }
    return sum;
}


char* state_pattern(const State *s) {
    int min = min_pot(s);
    int max = max_pot(s);
    int length = max - min + 1;
    char* pattern = (char*)malloc(sizeof(char) * (length + 1));
    
    for(int i = 0; i < length; i++){
        pattern[i] = state_get(s, min + i);
    }
    pattern[length] = '\0';
    return pattern;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Failed to open input.txt");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    char initialState[MAX_LINE_LENGTH];
    char rules[32][6] = {0};
    char results[32] = {0};
    int rule_count = 0;

    while (fgets(line, MAX_LINE_LENGTH, file)) {
        if (strstr(line, "initial state")) {
            sscanf(line, "initial state: %s", initialState);
        } else if (strstr(line, "=>")) {
            sscanf(line, "%s => %c", rules[rule_count], &results[rule_count]);
            rule_count++;
        }
    }
    fclose(file);

    State state = create_state();
    for (int i = 0; initialState[i] != '\0'; i++) {
        if (initialState[i] == '#') {
            state_add(&state, i, '#');
        }
    }

    char *previousPattern = NULL;
    long long previousSum = 0;
    long long offset = 0;


    for (long long generation = 0; generation < 50000000000; generation++) {
        State newState = create_state();
        int min = min_pot(&state);
        int max = max_pot(&state);

        for (int i = min - 2; i <= max + 2; i++) {
            char pattern[6] = {0};
            for (int j = i - 2; j <= i + 2; j++) {
                pattern[j - (i - 2)] = state_get(&state, j);
            }
          
            for(int k = 0; k < rule_count; k++){
                if(strcmp(pattern, rules[k]) == 0){
                     if (results[k] == '#') {
                        state_add(&newState, i, '#');
                      }
                     break;
                }
            }

        }
        free_state(&state);
        state = newState;

        qsort(state.pots, state.size, sizeof(Pot), compare_pots);
        
        char* currentPattern = state_pattern(&state);
        long long currentSum = state_sum(&state);

        if (previousPattern && strcmp(currentPattern, previousPattern) == 0) {
           
            offset = currentSum - previousSum;
            long long remainingGenerations = 50000000000 - generation - 1;
            long long finalSum = currentSum + offset * remainingGenerations;
            printf("%lld\n", finalSum);
            free(currentPattern);
            free(previousPattern);
            free_state(&state);
             return 0;
        }
        if(previousPattern){
            free(previousPattern);
        }
        previousPattern = currentPattern;
        previousSum = currentSum;
    }
    free_state(&state);
    return 0;
}
