
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    int x, y;
} Point;

typedef struct {
    int x, y, dist;
} QueueNode;

Point directions[] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

int h, w;
char **grid;
bool **walls;
Point S, E;
Point *trackCells;
int trackCellCount = 0;

void readInput() {
    FILE *f = fopen("input.txt", "r");
    if (f == NULL) {
        perror("Error opening file");
        exit(1);
    }

    char line[1024];
    int row = 0;
    int maxCol = 0;
    
    // Determine number of rows and max columns
     while (fgets(line, sizeof(line), f) != NULL) {
        row++;
        int col = strlen(line) - 1; // Exclude newline char
        if (col > maxCol)
          maxCol = col;
    }

    h = row;
    w = maxCol;

    grid = (char **)malloc(h * sizeof(char *));
    walls = (bool **)malloc(h * sizeof(bool *));
    trackCells = (Point *)malloc(h * w * sizeof(Point)); // Max track cells
    
    for (int i = 0; i < h; i++) {
        grid[i] = (char *)malloc((w + 1) * sizeof(char)); // +1 for null terminator
        walls[i] = (bool *)malloc(w * sizeof(bool));
        memset(walls[i], false, w * sizeof(bool));
    }
    
     fseek(f, 0, SEEK_SET); //Reset file pointer

    row = 0;
    while (fgets(line, sizeof(line), f) != NULL) {
        int len = strlen(line) - 1;
        strncpy(grid[row], line, len);
        grid[row][len] = '\0';
        for (int j = 0; j < w; j++) {
            if (j >= len){
                grid[row][j] = ' ';
            }
            char ch = grid[row][j];
            if (ch == 'S') {
                S.x = row;
                S.y = j;
            } else if (ch == 'E') {
                E.x = row;
                E.y = j;
            }
            if (ch == '#') {
                walls[row][j] = true;
            } else if (ch != ' '){
              trackCells[trackCellCount].x = row;
              trackCells[trackCellCount].y = j;
                trackCellCount++;
            }
        }
        row++;
    }
    fclose(f);
}

int **normalDistFrom(Point start) {
    int **dist = (int **)malloc(h * sizeof(int *));
    for (int i = 0; i < h; i++) {
        dist[i] = (int *)malloc(w * sizeof(int));
        for (int j = 0; j < w; j++) {
            dist[i][j] = -1;
        }
    }
    dist[start.x][start.y] = 0;

    QueueNode *queue = (QueueNode*) malloc(h*w * sizeof(QueueNode));
    int head = 0;
    int tail = 0;
    queue[tail++] = (QueueNode){start.x, start.y, 0};

    while (head < tail) {
        QueueNode cur = queue[head++];
        for (int i = 0; i < 4; i++) {
            int nx = cur.x + directions[i].x;
            int ny = cur.y + directions[i].y;
            if (nx < 0 || nx >= h || ny < 0 || ny >= w || walls[nx][ny] || dist[nx][ny] != -1) {
                continue;
            }
            dist[nx][ny] = cur.dist + 1;
            queue[tail++] = (QueueNode){nx, ny, cur.dist+1};
        }
    }
    free(queue);
    return dist;
}

bool isTrack(int x, int y) {
    return x >= 0 && x < h && y >= 0 && y < w && !walls[x][y];
}

int main() {
    readInput();

    int **distFromS = normalDistFrom(S);
    int **distFromE = normalDistFrom(E);

    if (distFromS[E.x][E.y] == -1) {
        printf("0\n");
        
         for(int i=0; i< h; i++) {
            free(distFromS[i]);
            free(distFromE[i]);
             free(grid[i]);
            free(walls[i]);
         }
        free(distFromS);
        free(distFromE);
        free(grid);
        free(walls);
        free(trackCells);
        return 0;
    }

    int normalCost = distFromS[E.x][E.y];
    int possibleCheats = 0;

    for (int t=0; t<trackCellCount; t++) {
        Point startPos = trackCells[t];
        int sd = distFromS[startPos.x][startPos.y];
        if (sd == -1) continue;
        
        for (int i = 0; i < 4; i++) {
            Point m1 = {startPos.x + directions[i].x, startPos.y + directions[i].y};
            if (m1.x < 0 || m1.x >= h || m1.y < 0 || m1.y >= w) continue;

            for (int j = 0; j < 4; j++) {
                Point m2 = {m1.x + directions[j].x, m1.y + directions[j].y};
                 if (m2.x < 0 || m2.x >= h || m2.y < 0 || m2.y >= w || !isTrack(m2.x, m2.y)) continue;
                 
                int ed = distFromE[m2.x][m2.y];
                if(ed == -1) continue;
                
                 int newCost = sd + 2 + ed;
                 if(normalCost - newCost >= 100)
                    possibleCheats++;
             }
         }
    }

    printf("%d\n", possibleCheats);
     for(int i=0; i< h; i++) {
            free(distFromS[i]);
            free(distFromE[i]);
             free(grid[i]);
            free(walls[i]);
         }
        free(distFromS);
        free(distFromE);
        free(grid);
        free(walls);
    free(trackCells);
    return 0;
}
