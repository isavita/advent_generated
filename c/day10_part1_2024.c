
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    int r, c;
} pos;

typedef struct {
    pos p;
    int h;
} queue_node;

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int nr = 0, nc = 0;
    char **lines = NULL;

    while ((read = getline(&line, &len, fp)) != -1) {
        line[strcspn(line, "\r\n")] = 0;
        lines = realloc(lines, sizeof(char*) * (nr + 1));
        lines[nr] = strdup(line);
        if (nr == 0) nc = strlen(line);
        nr++;
    }

    fclose(fp);
    if (line) free(line);
    
    int **grid = malloc(sizeof(int*) * nr);
    for (int i = 0; i < nr; i++) {
        grid[i] = malloc(sizeof(int) * nc);
        for (int j = 0; j < nc; j++) {
            grid[i][j] = lines[i][j] - '0';
        }
        free(lines[i]);
    }
    free(lines);

    pos dirs[] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
    pos *trailheads = NULL;
    int num_trailheads = 0;

    for (int r = 0; r < nr; r++) {
        for (int c = 0; c < nc; c++) {
            if (grid[r][c] == 0) {
                trailheads = realloc(trailheads, sizeof(pos) * (num_trailheads + 1));
                trailheads[num_trailheads].r = r;
                trailheads[num_trailheads].c = c;
                num_trailheads++;
            }
        }
    }

    int sumScores = 0;
    for (int i = 0; i < num_trailheads; i++) {
        pos th = trailheads[i];
        bool reached[nr * nc];
        memset(reached, false, sizeof(reached));
        queue_node *front = malloc(sizeof(queue_node) * nr * nc);
        int head = 0, tail = 0;
        front[tail++] = (queue_node){th, 0};
        bool visited[nr][nc][10];
        memset(visited, false, sizeof(visited));
        visited[th.r][th.c][0] = true;

        while (head < tail) {
            queue_node cur = front[head++];

            if (cur.h == 9) {
                if (!reached[cur.p.r * nc + cur.p.c]) {
                   reached[cur.p.r * nc + cur.p.c] = true;
                }
                continue;
            }
             
            for (int d = 0; d < 4; d++) {
                int nr2 = cur.p.r + dirs[d].r;
                int nc2 = cur.p.c + dirs[d].c;
                if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue;
                if (grid[nr2][nc2] == cur.h + 1) {
                     if(!visited[nr2][nc2][cur.h+1]){
                        visited[nr2][nc2][cur.h+1] = true;
                        front[tail++] = (queue_node){{nr2, nc2}, cur.h + 1};
                    }
                }
            }
        }
        free(front);
        int count = 0;
        for(int j = 0; j < nr * nc; j++){
            if(reached[j]) count++;
        }
        sumScores += count;
    }

    free(trailheads);
    for(int i = 0; i < nr; i++){
        free(grid[i]);
    }
    free(grid);
    printf("%d\n", sumScores);
    return 0;
}
