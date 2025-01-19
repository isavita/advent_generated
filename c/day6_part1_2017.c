
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_BANKS 16
#define MAX_STATES 100000

typedef struct {
    int banks[MAX_BANKS];
} State;

int banks_equal(State a, State b, int bank_count) {
    for (int i = 0; i < bank_count; ++i) {
        if (a.banks[i] != b.banks[i]) {
            return 0;
        }
    }
    return 1;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    int banks[MAX_BANKS];
    int bank_count = 0;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    
    if ((read = getline(&line, &len, fp)) != -1) {
        char *token = strtok(line, " \t\n\r");
        while (token != NULL && bank_count < MAX_BANKS) {
            banks[bank_count++] = atoi(token);
            token = strtok(NULL, " \t\n\r");
        }
    }
    free(line);
    fclose(fp);

    State seen_states[MAX_STATES];
    int seen_count = 0;
    int cycles = 0;
    
    State current_state;
    memcpy(current_state.banks, banks, sizeof(int) * bank_count);
    
    while(1) {
        int found_match = 0;
        for(int i = 0; i < seen_count; i++) {
          if(banks_equal(seen_states[i], current_state, bank_count)){
            found_match = 1;
            break;
          }
        }
      if(found_match){
        break;
      }
      
      seen_states[seen_count++] = current_state;

      int max_index = 0;
      for(int i = 1; i < bank_count; i++){
        if(current_state.banks[i] > current_state.banks[max_index]){
          max_index = i;
        }
      }
      
      int blocks = current_state.banks[max_index];
      current_state.banks[max_index] = 0;
      for(int i = 1; i <= blocks; i++){
        current_state.banks[(max_index + i) % bank_count]++;
      }
       
        cycles++;
    
    memcpy(banks,current_state.banks, sizeof(int) * bank_count);
    }
    
    printf("It takes %d redistribution cycles to reach a repeated configuration.\n", cycles);
    
    return 0;
}
