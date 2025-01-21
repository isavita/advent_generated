
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y;
} Coord;

typedef struct {
    int width, height;
    char **data;
} Grid;

typedef struct {
    Coord start, end;
    int weight;
} Edge;

typedef struct {
    Coord *vertices;
    int numVertices;
    Edge **edges;
    int *numEdges;
} Graph;

int max(int a, int b) {
    return a > b ? a : b;
}

int isValid(Grid grid, Coord coord) {
    return coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height && grid.data[coord.y][coord.x] != '#';
}

int isValidWithSlopes(Grid grid, Coord coord, Coord dir) {
    if (!isValid(grid, coord)) return 0;
    char c = grid.data[coord.y][coord.x];
    if (c == '^') return dir.y == -1;
    if (c == 'v') return dir.y == 1;
    if (c == '<') return dir.x == -1;
    if (c == '>') return dir.x == 1;
    return 1;
}

Coord add(Coord c1, Coord c2) {
    return (Coord){c1.x + c2.x, c1.y + c2.y};
}

Coord neighbors[] = {{0, -1}, {0, 1}, {-1, 0}, {1, 0}};

int isVertex(Grid grid, Coord coord, Graph *graph) {
    for (int i = 0; i < graph->numVertices; i++) {
        if (graph->vertices[i].x == coord.x && graph->vertices[i].y == coord.y) return 1;
    }
    return 0;
}

void getEdgesBFS(Grid grid, Coord start, Graph *graph, int (*isValidFunc)(Grid, Coord, Coord), int startIdx) {
    Coord *frontier = (Coord *)malloc(sizeof(Coord) * grid.width * grid.height);
    int *distances = (int *)malloc(sizeof(int) * grid.width * grid.height);
    char *reached = (char *)calloc(grid.width * grid.height, sizeof(char));
    int front = 0, rear = 0;

    frontier[rear++] = start;
    distances[start.y * grid.width + start.x] = 0;
    reached[start.y * grid.width + start.x] = 1;

    while (front < rear) {
        Coord current = frontier[front++];
        int currentIdx = current.y * grid.width + current.x;

        if (isVertex(grid, current, graph) && (current.x != start.x || current.y != start.y)) {
            graph->edges[startIdx] = (Edge *)realloc(graph->edges[startIdx], sizeof(Edge) * (graph->numEdges[startIdx] + 1));
            graph->edges[startIdx][graph->numEdges[startIdx]].start = start;
            graph->edges[startIdx][graph->numEdges[startIdx]].end = current;
            graph->edges[startIdx][graph->numEdges[startIdx]].weight = distances[currentIdx];
            graph->numEdges[startIdx]++;
            continue;
        }

        for (int i = 0; i < 4; i++) {
            Coord next = add(current, neighbors[i]);
            int nextIdx = next.y * grid.width + next.x;
            if (isValidFunc(grid, next, neighbors[i]) && !reached[nextIdx]) {
                frontier[rear++] = next;
                reached[nextIdx] = 1;
                distances[nextIdx] = distances[currentIdx] + 1;
            }
        }
    }

    free(frontier);
    free(distances);
    free(reached);
}

Graph getGraph(Grid grid, Coord start, Coord end, int (*isValidFunc)(Grid, Coord, Coord)) {
    Graph graph;
    graph.vertices = (Coord *)malloc(sizeof(Coord) * grid.width * grid.height);
    graph.numVertices = 0;
    graph.edges = (Edge **)malloc(sizeof(Edge *) * grid.width * grid.height);
    graph.numEdges = (int *)calloc(grid.width * grid.height, sizeof(int));

    graph.vertices[graph.numVertices++] = start;
    graph.vertices[graph.numVertices++] = end;

    for (int y = 0; y < grid.height; y++) {
        for (int x = 0; x < grid.width; x++) {
            if (grid.data[y][x] == '.') {
                Coord coord = {x, y};
                int validNeighbors = 0;
                for (int i = 0; i < 4; i++) {
                    if (isValid(grid, add(coord, neighbors[i]))) validNeighbors++;
                }
                if (validNeighbors > 2) {
                    graph.vertices[graph.numVertices++] = coord;
                }
            }
        }
    }
    graph.vertices = (Coord*) realloc(graph.vertices, sizeof(Coord) * graph.numVertices);
    graph.edges = (Edge**) realloc(graph.edges, sizeof(Edge*) * graph.numVertices);

    for (int i = 0; i < graph.numVertices; i++) {
        graph.edges[i] = NULL;
        getEdgesBFS(grid, graph.vertices[i], &graph, isValidFunc, i);
    }

    return graph;
}

int getMaxDistanceDFS(Graph graph, int current, int end, char *seen) {
    if (current == end) return 0;

    int maxi = -1;
    seen[current] = 1;

    for (int i = 0; i < graph.numEdges[current]; i++) {
        int next = -1;
        for(int j=0; j<graph.numVertices; ++j){
            if(graph.vertices[j].x == graph.edges[current][i].end.x && graph.vertices[j].y == graph.edges[current][i].end.y){
                next = j;
                break;
            }
        }

        if (!seen[next]) {
            int dist = getMaxDistanceDFS(graph, next, end, seen);
            if (dist >= 0) {
                maxi = max(maxi, dist + graph.edges[current][i].weight);
            }
        }
    }
    seen[current] = 0;

    return maxi >=0 ? maxi : -1;
}

int solve(char **input, int height) {
    int width = strlen(input[0]);
    Grid grid;
    grid.width = width;
    grid.height = height;
    grid.data = input;

    Coord start = {1, 0};
    Coord end = {width - 2, height - 1};

    Graph graph = getGraph(grid, start, end, isValidWithSlopes);

    char *seen = (char *)calloc(graph.numVertices, sizeof(char));
    int start_index = 0;
    int end_index = 1;

    int maxDist = getMaxDistanceDFS(graph, start_index, end_index, seen);
    
    free(seen);
    for(int i=0; i<graph.numVertices; ++i){
        free(graph.edges[i]);
    }
    free(graph.edges);
    free(graph.numEdges);
    free(graph.vertices);

    return maxDist;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char **input = (char **)malloc(sizeof(char *) * 200);
    int height = 0;
    char line[300];

    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0;
        input[height] = (char *)malloc(strlen(line) + 1);
        strcpy(input[height], line);
        height++;
    }

    fclose(file);

    printf("%d\n", solve(input, height));

    for (int i = 0; i < height; i++) {
        free(input[i]);
    }
    free(input);

    return 0;
}
