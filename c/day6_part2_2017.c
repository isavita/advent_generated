
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_BANKS 20
#define MAX_STATES 10000

typedef struct {
    int banks[MAX_BANKS];
    int size;
} State;

typedef struct {
    State states[MAX_STATES];
    int count;
} History;

bool state_equal(const State *s1, const State *s2) {
    if (s1->size != s2->size) return false;
    for (int i = 0; i < s1->size; i++) {
        if (s1->banks[i] != s2->banks[i]) return false;
    }
    return true;
}


int find_state(const History *history, const State *state) {
    for (int i = 0; i < history->count; i++) {
        if (state_equal(state, &history->states[i])) {
            return i;
        }
    }
    return -1;
}

void add_state(History *history, const State *state) {
     if (history->count < MAX_STATES) {
        history->states[history->count] = *state;
        history->count++;
    }
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    
    int banks[MAX_BANKS];
    int num, size = 0;
    
    while (fscanf(fp, "%d", &num) == 1 && size < MAX_BANKS){
        banks[size++] = num;
    }

    fclose(fp);

    History history;
    history.count = 0;
    
    State current_state;
    current_state.size = size;
    memcpy(current_state.banks, banks, sizeof(int) * size);

    int cycles = 0;
    int loop_start;


    while (true) {
       loop_start = find_state(&history, &current_state);

       if(loop_start != -1){
           printf("The size of the loop is %d\n", cycles - loop_start);
           break;
       }

       add_state(&history, &current_state);

        int max_index = 0;
        for (int i = 1; i < size; i++) {
            if (current_state.banks[i] > current_state.banks[max_index]) {
                max_index = i;
            }
        }

        int blocks = current_state.banks[max_index];
        current_state.banks[max_index] = 0;
        for (int i = 1; i <= blocks; i++) {
            current_state.banks[(max_index + i) % size]++;
        }
        
        cycles++;
    }

    return 0;
}
