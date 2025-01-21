
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_STATE_SIZE 2000
#define NUM_RULES 128
#define RULE_LENGTH 5

typedef struct {
    int key;
    char value;
} StateEntry;


int compareStateEntries(const void *a, const void *b) {
    return ((StateEntry *)a)->key - ((StateEntry *)b)->key;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char initialState[200] = {0};
    char rules[NUM_RULES][RULE_LENGTH+1] = {0};
    char ruleResults[NUM_RULES] = {0};
    int numRules = 0;
    
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        if (strstr(line, "initial state")) {
            sscanf(line, "initial state: %s", initialState);
        } else if (strstr(line, "=>")) {
             char pattern[RULE_LENGTH+1];
             char result;
             sscanf(line, "%s => %c", pattern, &result);
              strcpy(rules[numRules], pattern);
              ruleResults[numRules] = result;
              numRules++;
        }
    }

    fclose(file);

    StateEntry state[MAX_STATE_SIZE];
    int stateSize = 0;
    for (int i = 0; initialState[i] != '\0'; i++) {
        if (initialState[i] == '#') {
            state[stateSize].key = i;
            state[stateSize].value = '#';
            stateSize++;
        }
    }

     for (int generation = 0; generation < 20; generation++) {
        StateEntry newState[MAX_STATE_SIZE];
        int newStateSize = 0;
        
        if (stateSize == 0) continue;
        
        qsort(state, stateSize, sizeof(StateEntry), compareStateEntries);
        int minPot = state[0].key;
        int maxPot = state[stateSize - 1].key;


        for (int i = minPot - 2; i <= maxPot + 2; i++) {
            char pattern[RULE_LENGTH+1] = {'.', '.', '.', '.', '.', '\0'};
            for (int j = i - 2; j <= i + 2; j++) {
                bool found = false;
                for(int k=0; k < stateSize; k++)
                {
                   if(state[k].key == j)
                    {
                       pattern[j-(i-2)] = '#';
                       found = true;
                       break;
                    }
                }

            }
           
            for(int r =0; r<numRules; r++)
            {
                if(strcmp(pattern,rules[r]) == 0)
                {
                  if(ruleResults[r] == '#')
                  {
                      newState[newStateSize].key = i;
                      newState[newStateSize].value = '#';
                      newStateSize++;
                   }
                   break;
                }

            }
        }
       
        stateSize = newStateSize;
        memcpy(state, newState, sizeof(StateEntry) * stateSize);
    }

    long long sum = 0;
    for(int i=0; i < stateSize; i++)
    {
       sum+= state[i].key;
    }
   
    printf("%lld\n", sum);
    return 0;
}
