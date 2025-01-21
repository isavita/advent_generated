
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point pos;
    int step;
} State;

typedef struct {
    Point min, max;
} Rectangle;

int dx[] = {0, 0, 1, -1, 0};
int dy[] = {1, -1, 0, 0, 0};
char bliz[] = {'^', '>', 'v', '<'};

int steps(char** grid, int rows, int cols, Point start, Point end, int initialStep) {
    State* q = (State*)malloc(sizeof(State) * rows * cols * 100);
    int head = 0, tail = 0;
    q[tail++] = (State){start, initialStep};

    char*** seen = (char***)malloc(sizeof(char**) * rows);
    for (int i = 0; i < rows; i++) {
        seen[i] = (char**)malloc(sizeof(char*) * cols);
        for (int j = 0; j < cols; j++) {
            seen[i][j] = (char*)calloc(rows * cols, sizeof(char));
        }
    }

    while (head < tail) {
        State curr = q[head++];
        if (curr.pos.x == end.x && curr.pos.y == end.y) {
             for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    free(seen[i][j]);
                }
                free(seen[i]);
            }
            free(seen);
            free(q);
            return curr.step;
        }

        for (int i = 0; i < 5; i++) {
            Point next_pos = {curr.pos.x + dx[i], curr.pos.y + dy[i]};
            int next_step = curr.step + 1;

            if (next_pos.x < 0 || next_pos.x >= cols || next_pos.y < 0 || next_pos.y >= rows || seen[next_pos.y][next_pos.x][next_step % (rows * cols)] || grid[next_pos.y][next_pos.x] == '#') {
                continue;
            }
            
            if (next_pos.y > 0 && next_pos.y < rows - 1) {
                int valid = 1;
                 for (int j = 0; j < 4; j++) {
                    int prev_x, prev_y;
                    if (bliz[j] == '^') {
                        prev_x = next_pos.x;
                        prev_y = (next_pos.y + next_step) % (rows - 2);
                        if (prev_y == 0) prev_y = rows-2;
                        
                    } else if (bliz[j] == '>') {
                        prev_x = (next_pos.x - next_step) % (cols - 2);
                        if(prev_x < 0) prev_x = prev_x + cols - 2;
                        if(prev_x == 0) prev_x = cols - 2;
                        prev_y = next_pos.y;
                    } else if (bliz[j] == 'v') {
                         prev_x = next_pos.x;
                        prev_y = (next_pos.y - next_step) % (rows - 2);
                         if(prev_y < 0) prev_y = prev_y + rows - 2;
                        if(prev_y == 0) prev_y = rows - 2;
                       
                    } else {
                         prev_x = (next_pos.x + next_step) % (cols - 2);
                        if(prev_x == 0) prev_x = cols - 2;
                        prev_y = next_pos.y;
                    }

                    if (grid[prev_y][prev_x] == bliz[j]) {
                        valid = 0;
                        break;
                    }
                }
                if(!valid) continue;
            }

            seen[next_pos.y][next_pos.x][next_step % (rows * cols)] = 1;
            q[tail++] = (State){next_pos, next_step};
        }
    }
      for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    free(seen[i][j]);
                }
                free(seen[i]);
            }
            free(seen);
            free(q);
    return -1;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char** grid = (char**)malloc(sizeof(char*) * 200);
    int rows = 0;
    char line[300];
    int cols = 0;
    while (fgets(line, sizeof(line), file)) {
        
        int len = strlen(line);
         if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
             len--;
        }
        if(cols == 0) cols = len;
        grid[rows] = (char*)malloc(sizeof(char) * (len + 1));
        
        for(int i = 0; i < len; i++){
            grid[rows][i] = line[i];
        }
        grid[rows][len] = '\0';
        rows++;
    }
    fclose(file);

    Point entrance = {1, 0};
    Point exit = {cols - 2, rows - 1};

    int crossing = steps(grid, rows, cols, entrance, exit, 0);

    printf("%d\n", crossing);

    for (int i = 0; i < rows; i++) {
        free(grid[i]);
    }
    free(grid);

    return 0;
}
