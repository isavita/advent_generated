
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_PROGRAMS 2000
#define MAX_LINE_LENGTH 100

int adjMatrix[MAX_PROGRAMS][MAX_PROGRAMS];
bool visited[MAX_PROGRAMS];
int numPrograms = 0;

void dfs(int program) {
    visited[program] = true;
    for (int i = 0; i < numPrograms; i++) {
        if (adjMatrix[program][i] && !visited[i]) {
            dfs(i);
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        int program;
        sscanf(line, "%d <->", &program);
        if(program >= numPrograms)
            numPrograms = program + 1;
        
        char *connections = strchr(line, '>');
        if(connections){
            connections += 2;
        }
        
        char *token = strtok(connections, ", ");
        while (token != NULL) {
            int connectedProgram = atoi(token);
            if(connectedProgram >= numPrograms)
                numPrograms = connectedProgram + 1;
            
            adjMatrix[program][connectedProgram] = 1;
            adjMatrix[connectedProgram][program] = 1;
           
            token = strtok(NULL, ", ");
        }
    }
    fclose(file);

    dfs(0);

    int count = 0;
    for (int i = 0; i < numPrograms; i++) {
        if (visited[i]) {
            count++;
        }
    }

    printf("%d\n", count);

    return 0;
}
