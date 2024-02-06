
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STEPS 26

int graph[MAX_STEPS][MAX_STEPS];
int in_degree[MAX_STEPS];

int main() {
    FILE *fp = fopen("input.txt", "r");
    
    char step1, step2;
    while (fscanf(fp, "Step %c must be finished before step %c can begin.\n", &step1, &step2) != EOF) {
        graph[step1 - 'A'][step2 - 'A'] = 1;
        in_degree[step2 - 'A']++;
    }
    
    fclose(fp);
    
    int completed = 0;
    while (completed < MAX_STEPS) {
        for (int i = 0; i < MAX_STEPS; i++) {
            if (in_degree[i] == 0) {
                printf("%c", i + 'A');
                in_degree[i] = -1;
                completed++;
                
                for (int j = 0; j < MAX_STEPS; j++) {
                    if (graph[i][j] == 1) {
                        in_degree[j]--;
                    }
                }
                
                break;
            }
        }
    }
    
    return 0;
}
