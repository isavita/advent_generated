
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#define MAX_H 200
#define MAX_W 150
#define MAX_P 8192

char map[MAX_H][MAX_W];
char path[MAX_P];
int width = 0, height = 0;

typedef struct {
    int r, c, d;
} Pos;

Pos wrap(int r, int c, int d) {
    if (d == 3 && r == 0 && c >= 50 && c < 100)   return (Pos){c + 100, 0, 0};
    if (d == 2 && c == 50 && r >= 0 && r < 50)    return (Pos){149 - r, 0, 0};
    if (d == 3 && r == 0 && c >= 100 && c < 150)  return (Pos){199, c - 100, 3};
    if (d == 0 && c == 149 && r >= 0 && r < 50)   return (Pos){149 - r, 99, 2};
    if (d == 1 && r == 49 && c >= 100 && c < 150) return (Pos){c - 50, 99, 2};
    if (d == 0 && c == 99 && r >= 50 && r < 100)  return (Pos){49, r + 50, 3};
    if (d == 2 && c == 50 && r >= 50 && r < 100)  return (Pos){100, r - 50, 1};
    if (d == 2 && c == 0 && r >= 100 && r < 150)  return (Pos){149 - r, 50, 0};
    if (d == 3 && r == 100 && c >= 0 && c < 50)   return (Pos){c + 50, 50, 0};
    if (d == 0 && c == 99 && r >= 100 && r < 150) return (Pos){149 - r, 149, 2};
    if (d == 1 && r == 149 && c >= 50 && c < 100) return (Pos){c + 100, 49, 2};
    if (d == 0 && c == 49 && r >= 150 && r < 200) return (Pos){149, r - 100, 3};
    if (d == 1 && r == 199 && c >= 0 && c < 50)   return (Pos){0, c + 100, 1};
    if (d == 2 && c == 0 && r >= 150 && r < 200)  return (Pos){0, r - 100, 1};
    return (Pos){r, c, d};
}

int main(void) {
    FILE *fp = fopen("input.txt", "r");
    memset(map, ' ', sizeof(map));
    char line[MAX_W + 2];
    while (fgets(line, sizeof(line), fp) && line[0] != '\n') {
        int len = strcspn(line, "\r\n");
        if (len > width) width = len;
        memcpy(map[height++], line, len);
    }
    fgets(path, MAX_P, fp);
    path[strcspn(path, "\r\n")] = 0;
    fclose(fp);

    int r = 0, c = 0, d = 0;
    while (map[r][c] != '.') c++;

    int dr[] = {0, 1, 0, -1};
    int dc[] = {1, 0, -1, 0};

    char *p = path;
    while (*p) {
        if (isdigit(*p)) {
            char *next_p;
            long steps = strtol(p, &next_p, 10);
            p = next_p;
            for (int i = 0; i < steps; ++i) {
                int nr = r + dr[d], nc = c + dc[d], nd = d;
                if (nr < 0 || nr >= height || nc < 0 || nc >= width || map[nr][nc] == ' ') {
                    Pos next_pos = wrap(r, c, d);
                    nr = next_pos.r; nc = next_pos.c; nd = next_pos.d;
                }
                if (map[nr][nc] == '#') break;
                r = nr; c = nc; d = nd;
            }
        } else {
            if (*p == 'R') d = (d + 1) % 4;
            else if (*p == 'L') d = (d + 3) % 4;
            p++;
        }
    }
    printf("%d\n", 1000 * (r + 1) + 4 * (c + 1) + d);
    return 0;
}
