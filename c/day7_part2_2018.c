
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

#define MAX_STEPS 26
#define NUM_WORKERS 5
#define BASE_DURATION 60

typedef struct {
    char id;
    int duration;
} Step;

typedef struct {
    char steps[MAX_STEPS];
    int count;
} StepList;

typedef struct {
    Step* steps[MAX_STEPS];
    int stepCount;
} AllSteps;

typedef struct {
    StepList deps[MAX_STEPS];
    AllSteps allSteps;
} Data;

Data parseInput(const char* filename);
int simulateWork(Data data);
bool isBeingWorkedOn(char step, char tasks[NUM_WORKERS]);
int findMinDuration(int durations[NUM_WORKERS]);
void finishStep(Data* data, char step);
void removeChar(char* str, char c);
int charToIndex(char c);


int main() {
    Data data = parseInput("input.txt");
    int timeTaken = simulateWork(data);
    printf("%d\n", timeTaken);
    return 0;
}


Data parseInput(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    Data data;
    memset(data.deps, 0, sizeof(data.deps));
    data.allSteps.stepCount = 0;
    memset(data.allSteps.steps, 0, sizeof(data.allSteps.steps));

    char line[100];
    while (fgets(line, sizeof(line), file)) {
        char a, b;
        sscanf(line, "Step %c must be finished before step %c can begin.", &a, &b);
        
        int index_a = charToIndex(a);
        int index_b = charToIndex(b);

        bool a_exists = false;
        for (int i = 0; i < data.allSteps.stepCount; i++) {
            if(data.allSteps.steps[i] && data.allSteps.steps[i]->id == a){
                a_exists = true;
                break;
            }
        }

        if (!a_exists) {
            Step* step_a = (Step*)malloc(sizeof(Step));
            step_a->id = a;
            step_a->duration = (a - 'A') + BASE_DURATION + 1;
            data.allSteps.steps[data.allSteps.stepCount++] = step_a;
        }


         bool b_exists = false;
        for (int i = 0; i < data.allSteps.stepCount; i++) {
            if(data.allSteps.steps[i] && data.allSteps.steps[i]->id == b){
                b_exists = true;
                 break;
            }
        }
        if (!b_exists) {
            Step* step_b = (Step*)malloc(sizeof(Step));
            step_b->id = b;
            step_b->duration = (b - 'A') + BASE_DURATION + 1;
            data.allSteps.steps[data.allSteps.stepCount++] = step_b;
        }

        data.deps[index_b].steps[data.deps[index_b].count++] = a;

    }

    fclose(file);
    return data;
}

int simulateWork(Data data) {
    int workers[NUM_WORKERS] = {0};
    char tasks[NUM_WORKERS] = {0};
    int time = 0;
    
    int allStepsCount = data.allSteps.stepCount;
    Step* allSteps[MAX_STEPS] ;
    memcpy(allSteps, data.allSteps.steps,sizeof(allSteps) );


    while (allStepsCount > 0) {
        char available[MAX_STEPS];
        int availableCount = 0;
         for(int i=0; i<MAX_STEPS; i++){
            if(allSteps[i]){
               int index = charToIndex(allSteps[i]->id);
                if (data.deps[index].count == 0 && !isBeingWorkedOn(allSteps[i]->id, tasks)) {
                    available[availableCount++] = allSteps[i]->id;
                }
            }
         }
         

        for (int i = 0; i < NUM_WORKERS; i++) {
            if (workers[i] == 0 && availableCount > 0) {
                char nextTask = ' ';
                char minChar = 'Z' + 1;
                int minIndex = -1;

                for(int j=0; j<availableCount; j++){
                   if(available[j]<minChar){
                    minChar = available[j];
                    minIndex= j;
                   }
                }
                nextTask = available[minIndex];
                available[minIndex] = available[--availableCount];

                tasks[i] = nextTask;
                  for(int j=0; j<MAX_STEPS; j++){
                      if(allSteps[j] && allSteps[j]->id == nextTask){
                         workers[i] = allSteps[j]->duration;
                         break;
                     }
                }
            }
        }


        int minDuration = findMinDuration(workers);
        for (int i = 0; i < NUM_WORKERS; i++) {
            if (workers[i] != 0) {
                workers[i] -= minDuration;
                if (workers[i] == 0) {
                   finishStep(&data,tasks[i]);
                    for (int j = 0; j < MAX_STEPS; j++) {
                        if(allSteps[j] && allSteps[j]->id == tasks[i]){
                            free(allSteps[j]);
                            allSteps[j] = NULL;
                            allStepsCount--;
                            break;
                        }
                    }
                   tasks[i] = 0;
                }
            }
        }
        time += minDuration;
    }

    return time;
}

bool isBeingWorkedOn(char step, char tasks[NUM_WORKERS]) {
    for (int i = 0; i < NUM_WORKERS; i++) {
        if (tasks[i] == step) {
            return true;
        }
    }
    return false;
}

int findMinDuration(int durations[NUM_WORKERS]) {
    int min = INT_MAX;
    for (int i = 0; i < NUM_WORKERS; i++) {
        if (durations[i] > 0 && durations[i] < min) {
            min = durations[i];
        }
    }
    return min == INT_MAX ? 1 : min;
}

void finishStep(Data* data, char step) {
    int stepIndex = charToIndex(step);
    for(int i=0; i< MAX_STEPS; i++){
        if (data->deps[i].count >0){
           removeChar(data->deps[i].steps,step);
            int count = 0;
            for (int j=0; j<MAX_STEPS; j++){
                if(data->deps[i].steps[j]){
                    data->deps[i].steps[count++] = data->deps[i].steps[j];
                 }
                
            }
            data->deps[i].count = count;
        }
    }

}

void removeChar(char* str, char c) {
    int j = 0;
    for (int i = 0; str[i]; i++) {
        if (str[i] != c) {
            str[j++] = str[i];
        }
    }
    str[j] = '\0';
}

int charToIndex(char c) {
    return c - 'A';
}
