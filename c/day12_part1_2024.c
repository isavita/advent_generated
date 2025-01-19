
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    int x, y;
} Point;

int solve(char** grid, int rows, int cols);
void calculateRegion(char** grid, int row, int col, int rows, int cols, bool** visited, int* area, int* perimeter);

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char** grid = NULL;
    size_t line_size = 0;
    char* line = NULL;
    int rows = 0;
    
    while (getline(&line, &line_size, file) != -1) {
        size_t len = strlen(line);
        if(len > 0 && line[len-1] == '\n'){
            line[len -1] = '\0';
            len--;
        }
        grid = realloc(grid, sizeof(char*) * (rows + 1));
        grid[rows] = malloc(sizeof(char) * (len+1));
        strcpy(grid[rows], line);
        rows++;
    }
    free(line);
    fclose(file);
    
    int cols = rows > 0 ? strlen(grid[0]) : 0;

    int totalPrice = solve(grid, rows, cols);
    printf("%d\n", totalPrice);

    for (int i = 0; i < rows; i++) {
        free(grid[i]);
    }
    free(grid);

    return 0;
}

int solve(char** grid, int rows, int cols) {
    if (rows == 0) return 0;

    bool** visited = malloc(sizeof(bool*) * rows);
    for (int i = 0; i < rows; i++) {
        visited[i] = malloc(sizeof(bool) * cols);
        memset(visited[i], false, sizeof(bool) * cols);
    }

    int totalPrice = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (!visited[r][c]) {
                int area = 0, perimeter = 0;
                calculateRegion(grid, r, c, rows, cols, visited, &area, &perimeter);
                totalPrice += area * perimeter;
            }
        }
    }

     for (int i = 0; i < rows; i++) {
        free(visited[i]);
    }
    free(visited);

    return totalPrice;
}

void calculateRegion(char** grid, int row, int col, int rows, int cols, bool** visited, int* area, int* perimeter) {
    char char_val = grid[row][col];
    *area = 0;
    *perimeter = 0;

    Point* queue = malloc(sizeof(Point) * rows * cols);
    int head = 0, tail = 0;

    queue[tail].x = row;
    queue[tail].y = col;
    tail++;
    visited[row][col] = true;

    while (head < tail) {
        Point p = queue[head++];
        (*area)++;

        bool isBorder = (p.x == 0 || p.x == rows - 1 || p.y == 0 || p.y == cols - 1);

        // Check top
        if (p.x > 0) {
            if (grid[p.x - 1][p.y] != char_val) {
                (*perimeter)++;
            } else if (!visited[p.x - 1][p.y]) {
                queue[tail].x = p.x - 1;
                queue[tail].y = p.y;
                tail++;
                visited[p.x - 1][p.y] = true;
            }
        } else if (isBorder) {
            (*perimeter)++;
        }

        // Check bottom
        if (p.x < rows - 1) {
            if (grid[p.x + 1][p.y] != char_val) {
                (*perimeter)++;
            } else if (!visited[p.x + 1][p.y]) {
               queue[tail].x = p.x + 1;
               queue[tail].y = p.y;
               tail++;
                visited[p.x + 1][p.y] = true;
            }
        } else if (isBorder) {
           (*perimeter)++;
        }

        // Check left
        if (p.y > 0) {
            if (grid[p.x][p.y - 1] != char_val) {
                (*perimeter)++;
            } else if (!visited[p.x][p.y - 1]) {
                queue[tail].x = p.x;
                queue[tail].y = p.y - 1;
                tail++;
                visited[p.x][p.y - 1] = true;
            }
        } else if (isBorder) {
            (*perimeter)++;
        }

        // Check right
        if (p.y < cols - 1) {
            if (grid[p.x][p.y + 1] != char_val) {
                (*perimeter)++;
            } else if (!visited[p.x][p.y + 1]) {
               queue[tail].x = p.x;
               queue[tail].y = p.y + 1;
               tail++;
               visited[p.x][p.y + 1] = true;
            }
        } else if (isBorder) {
             (*perimeter)++;
        }
    }
     free(queue);
}
