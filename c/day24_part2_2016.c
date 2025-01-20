
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

int min(int a, int b) {
    return a < b ? a : b;
}

int bfs(char** grid, int rows, int cols, int start_row, int start_col, int* distances, int poi_count) {
    int** queue = (int**)malloc(rows * cols * sizeof(int*));
    for (int i = 0; i < rows * cols; i++) {
        queue[i] = (int*)malloc(3 * sizeof(int));
    }
    int head = 0, tail = 0;
    queue[tail][0] = start_row;
    queue[tail][1] = start_col;
    queue[tail][2] = 0;
    tail++;
    int** visited = (int**)malloc(rows * sizeof(int*));
    for (int i = 0; i < rows; i++) {
        visited[i] = (int*)calloc(cols, sizeof(int));
    }
    visited[start_row][start_col] = 1;
    int dr[] = {0, 0, 1, -1};
    int dc[] = {-1, 1, 0, 0};
    while (head < tail) {
        int r = queue[head][0];
        int c = queue[head][1];
        int d = queue[head][2];
        head++;
        if (grid[r][c] >= '0' && grid[r][c] <= '9') {
            distances[grid[r][c] - '0'] = d;
        }
        for (int i = 0; i < 4; i++) {
            int nr = r + dr[i];
            int nc = c + dc[i];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#' && !visited[nr][nc]) {
                visited[nr][nc] = 1;
                queue[tail][0] = nr;
                queue[tail][1] = nc;
                queue[tail][2] = d + 1;
                tail++;
            }
        }
    }
    for (int i = 0; i < rows * cols; i++) {
        free(queue[i]);
    }
    free(queue);
    for (int i = 0; i < rows; i++) {
        free(visited[i]);
    }
    free(visited);
    
    return 0;
}

int dfs(int** graph, int entry, int* visited, int poi_count, int return_to_zero) {
    int visited_count = 0;
    for(int i = 0; i < poi_count; i++){
        if(visited[i]) visited_count++;
    }

    if (visited_count == poi_count) {
        return return_to_zero ? graph[entry][0] : 0;
    }
    int min_dist = INT_MAX;
    for (int i = 0; i < poi_count; i++) {
        if (!visited[i]) {
            visited[i] = 1;
            int dist = graph[entry][i] + dfs(graph, i, visited, poi_count, return_to_zero);
            min_dist = min(min_dist, dist);
            visited[i] = 0;
        }
    }
    return min_dist;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    char** grid = (char**)malloc(1000 * sizeof(char*));
    for (int i = 0; i < 1000; i++) {
        grid[i] = (char*)malloc(1000 * sizeof(char));
    }
    int rows = 0, cols = 0;
    while (fgets(grid[rows], 1000, file)) {
        grid[rows][strcspn(grid[rows], "\n")] = 0;
        cols = strlen(grid[rows]);
        rows++;
    }
    fclose(file);
    int poi_count = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] >= '0' && grid[r][c] <= '9') {
                poi_count++;
            }
        }
    }
    int** graph = (int**)malloc(poi_count * sizeof(int*));
    for (int i = 0; i < poi_count; i++) {
        graph[i] = (int*)malloc(poi_count * sizeof(int));
    }
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] >= '0' && grid[r][c] <= '9') {
                int distances[poi_count];
                bfs(grid, rows, cols, r, c, distances, poi_count);
                int index = grid[r][c] - '0';
                for(int i = 0; i < poi_count; i++){
                  graph[index][i] = distances[i];
                }
            }
        }
    }
    int* visited = (int*)calloc(poi_count, sizeof(int));
    visited[0] = 1;
    int result = dfs(graph, 0, visited, poi_count, 1);
    printf("%d\n", result);
    for (int i = 0; i < 1000; i++) {
        free(grid[i]);
    }
    free(grid);
    for (int i = 0; i < poi_count; i++) {
        free(graph[i]);
    }
    free(graph);
    free(visited);
    return 0;
}
