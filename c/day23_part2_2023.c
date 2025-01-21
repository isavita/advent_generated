
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
    int vertices_count;
    Coord *vertices;
    int edges_count;
    Edge *edges;
    int **adj;
} Graph;

Coord North = {0, -1};
Coord South = {0, 1};
Coord West = {-1, 0};
Coord East = {1, 0};

char Empty = '.';
char Wall = '#';

int max(int a, int b) {
    return (a > b) ? a : b;
}

int isInBounds(Grid *grid, Coord coord) {
    return 0 <= coord.x && coord.x < grid->width && 0 <= coord.y && coord.y < grid->height;
}

Grid parseInput(char **input, int height) {
    Grid grid;
    grid.width = strlen(input[0]);
    grid.height = height;
    grid.data = (char **)malloc(grid.height * sizeof(char *));
    for (int i = 0; i < grid.height; i++) {
        grid.data[i] = (char *)malloc(grid.width * sizeof(char));
        for (int j = 0; j < grid.width; j++) {
            grid.data[i][j] = input[i][j];
        }
    }
    return grid;
}

int isValidNeighbor(Grid *grid, Coord coord) {
    if (!isInBounds(grid, coord)) {
        return 0;
    }
    if (grid->data[coord.y][coord.x] == Wall) {
        return 0;
    }
    return 1;
}

Coord addCoord(Coord c1, Coord c2) {
    return (Coord){c1.x + c2.x, c1.y + c2.y};
}

Coord *neighbors4(Grid *grid, Coord coord, int *count) {
    Coord directions[] = {North, South, West, East};
    Coord *validNeighbors = (Coord *)malloc(4 * sizeof(Coord));
    *count = 0;

    for (int i = 0; i < 4; i++) {
        Coord neighbor = addCoord(coord, directions[i]);
        if (isValidNeighbor(grid, neighbor)) {
            validNeighbors[*count] = neighbor;
            (*count)++;
        }
    }
    return validNeighbors;
}

int isVertexInGraph(Graph *graph, Coord coord) {
    for (int i = 0; i < graph->vertices_count; i++) {
        if (graph->vertices[i].x == coord.x && graph->vertices[i].y == coord.y) {
            return 1;
        }
    }
    return 0;
}
int findVertexIndex(Graph *graph, Coord coord){
    for(int i = 0; i < graph->vertices_count; i++){
        if(graph->vertices[i].x == coord.x && graph->vertices[i].y == coord.y){
            return i;
        }
    }
    return -1;
}

Graph getGraph(Grid *grid, Coord start, Coord end) {
    Graph graph;
    graph.vertices_count = 0;
    graph.vertices = (Coord *)malloc(sizeof(Coord));
    graph.edges_count = 0;
    graph.edges = (Edge *)malloc(sizeof(Edge));

    graph.vertices[graph.vertices_count++] = start;
    graph.vertices = (Coord *)realloc(graph.vertices, (graph.vertices_count + 1) * sizeof(Coord));
    graph.vertices[graph.vertices_count++] = end;
    graph.vertices = (Coord *)realloc(graph.vertices, (graph.vertices_count + 1) * sizeof(Coord));

    for (int y = 0; y < grid->height; y++) {
        for (int x = 0; x < grid->width; x++) {
            Coord coord = {x, y};
            int neighbor_count;
            if (grid->data[y][x] == Empty) {
                neighbors4(grid, coord, &neighbor_count);
                if (neighbor_count > 2) {
                    if (!isVertexInGraph(&graph, coord)) {
                         graph.vertices[graph.vertices_count++] = coord;
                        graph.vertices = (Coord *)realloc(graph.vertices, (graph.vertices_count + 1) * sizeof(Coord));
                    }
                   
                }
            }
        }
    }
     graph.adj = (int **)malloc(graph.vertices_count * sizeof(int *));
    for (int i = 0; i < graph.vertices_count; i++) {
        graph.adj[i] = (int *)malloc(graph.vertices_count * sizeof(int));
        for (int j = 0; j < graph.vertices_count; j++) {
            graph.adj[i][j] = 0;
        }
    }

    for (int i = 0; i < graph.vertices_count; i++) {
        Coord start_vertex = graph.vertices[i];
        Coord *queue = (Coord *)malloc(grid->width * grid->height * sizeof(Coord));
        int front = 0, rear = 0;
        queue[rear++] = start_vertex;

        int *distances = (int *)malloc(grid->width * grid->height * sizeof(int));
        memset(distances, -1, grid->width * grid->height * sizeof(int));
        distances[start_vertex.y * grid->width + start_vertex.x] = 0;

        char **visited = (char **)malloc(grid->height * sizeof(char *));
        for(int j=0; j<grid->height; ++j){
            visited[j] = (char *)malloc(grid->width * sizeof(char));
            memset(visited[j], 0, grid->width * sizeof(char));
        }
        visited[start_vertex.y][start_vertex.x] = 1;

        while (front < rear) {
            Coord current = queue[front++];

            if (isVertexInGraph(&graph, current) && !(current.x == start_vertex.x && current.y == start_vertex.y)) {
                int u = findVertexIndex(&graph, start_vertex);
                int v = findVertexIndex(&graph, current);
                 graph.adj[u][v] = distances[current.y * grid->width + current.x];
            } else {
                int neighbor_count;
                Coord *neighbors = neighbors4(grid, current, &neighbor_count);
                for (int j = 0; j < neighbor_count; j++) {
                    Coord next = neighbors[j];
                    if (!visited[next.y][next.x]) {
                        visited[next.y][next.x] = 1;
                        queue[rear++] = next;
                        distances[next.y * grid->width + next.x] = distances[current.y * grid->width + current.x] + 1;
                    }
                }
                free(neighbors);
            }
        }
         for(int j=0; j<grid->height; ++j){
            free(visited[j]);
        }
        free(visited);
        free(queue);
        free(distances);
    }

    return graph;
}

int getMaxDistanceDFS(Graph *graph, int u, int end_index, char *seen, int *dist_arr) {
    if (u == end_index) {
        return 0;
    }

    seen[u] = 1;
    int max_dist = -1;

    for (int v = 0; v < graph->vertices_count; v++) {
        if (graph->adj[u][v] > 0 && !seen[v]) {
            int dist = getMaxDistanceDFS(graph, v, end_index, seen, dist_arr);
            if (dist != -1) {
               
                max_dist = max(max_dist, dist + graph->adj[u][v]);
            }
        }
    }

    seen[u] = 0;
    return max_dist == -1 ? -1 : max_dist;
}

int solve(char **input, int height) {
    Grid grid = parseInput(input, height);

    Coord start = {1, 0};
    Coord end = {grid.width - 2, grid.height - 1};

    Graph graph = getGraph(&grid, start, end);

    char *seen = (char *)calloc(graph.vertices_count, sizeof(char));
     int *dist_arr = (int *)calloc(graph.vertices_count, sizeof(int));
    int maxDist = getMaxDistanceDFS(&graph, 0, 1, seen, dist_arr);

    free(seen);
    for (int i = 0; i < grid.height; i++) {
        free(grid.data[i]);
    }
    free(grid.data);

    for (int i = 0; i < graph.vertices_count; i++) {
        free(graph.adj[i]);
    }
    free(graph.adj);
    free(graph.vertices);
    free(graph.edges);
     free(dist_arr);

    return maxDist;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char **lines = (char **)malloc(200 * sizeof(char *));
    int lineCount = 0;
    char buffer[200];

    while (fgets(buffer, sizeof(buffer), file)) {
        buffer[strcspn(buffer, "\n")] = 0;
        lines[lineCount] = (char *)malloc((strlen(buffer) + 1) * sizeof(char));
        strcpy(lines[lineCount], buffer);
        lineCount++;
    }

    fclose(file);

    int result = solve(lines, lineCount);
    printf("%d\n", result);

    for (int i = 0; i < lineCount; i++) {
        free(lines[i]);
    }
    free(lines);

    return 0;
}
