
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_NODES 20
#define MAX_NAME_LEN 10

typedef struct {
    char* nodes[MAX_NODES];
    int adjMatrix[MAX_NODES][MAX_NODES];
    int nodeCount;
} Graph;

int findOrAddNode(Graph* graph, const char* name) {
    for (int i = 0; i < graph->nodeCount; ++i) {
        if (strcmp(graph->nodes[i], name) == 0) {
            return i;
        }
    }
    graph->nodes[graph->nodeCount] = strdup(name);
    return graph->nodeCount++;
}

void addEdge(Graph* graph, const char* from, const char* to) {
    int fromIndex = findOrAddNode(graph, from);
    int toIndex = findOrAddNode(graph, to);
    graph->adjMatrix[fromIndex][toIndex] = 1;
    graph->adjMatrix[toIndex][fromIndex] = 1;
}

bool isLowerCase(const char* str) {
    for (int i = 0; str[i] != '\0'; ++i) {
        if (str[i] < 'a' || str[i] > 'z') return false;
    }
    return true;
}

int walk(Graph* graph, int current, int* visited, bool doubleUsed) {
    if (strcmp(graph->nodes[current], "end") == 0) return 1;
    visited[current]++;
    int pathsToEnd = 0;

    for (int next = 0; next < graph->nodeCount; ++next) {
        if (graph->adjMatrix[current][next] == 0) continue;
        if (strcmp(graph->nodes[next], "start") == 0) continue;
        if(isLowerCase(graph->nodes[next]) && visited[next] > 0){
            if(doubleUsed) continue;
             else doubleUsed = true;
        }

        pathsToEnd += walk(graph, next, visited, doubleUsed);
         visited[next]--;

         if(isLowerCase(graph->nodes[next]) && visited[next] ==1)
            doubleUsed = false;

    }
    return pathsToEnd;
}


int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Graph graph;
    graph.nodeCount = 0;
    memset(graph.adjMatrix, 0, sizeof(graph.adjMatrix));


    char line[100];
    while (fgets(line, sizeof(line), file)) {
        char* token = strtok(line, "-\n");
        char* from = token;
        token = strtok(NULL, "-\n");
        char* to = token;

        addEdge(&graph, from, to);
    }
    fclose(file);

    int visited[MAX_NODES] = {0};
    int startNode = findOrAddNode(&graph, "start");
    printf("%d\n", walk(&graph, startNode, visited, false));
    
    for (int i = 0; i < graph.nodeCount; ++i) {
        free(graph.nodes[i]);
    }
    return 0;
}
