
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int r, c;
} pos;

const pos dirs[] = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};
int nr, nc;
int** grid;
long long** dp;

long long dfs(int r, int c) {
    if (dp[r][c] != -1) {
        return dp[r][c];
    }
    int h = grid[r][c];
    if (h == 9) {
        dp[r][c] = 1;
        return 1;
    }
    long long sum = 0;
    for (int i = 0; i < 4; i++) {
        int nr2 = r + dirs[i].r;
        int nc2 = c + dirs[i].c;
        if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue;
        if (grid[nr2][nc2] == h + 1) {
            sum += dfs(nr2, nc2);
        }
    }
    dp[r][c] = sum;
    return sum;
}

int main() {
    FILE* fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char* line = NULL;
    size_t len = 0;
    ssize_t read;
    int row = 0;
    
    
    while ((read = getline(&line, &len, fp)) != -1) {
        if (row == 0) {
             nc = strlen(line) - 1;
             
        }
        row++;
    }
    
    nr = row;
    
    fclose(fp);

    grid = (int**)malloc(nr * sizeof(int*));
    dp = (long long**)malloc(nr * sizeof(long long*));
    for (int i = 0; i < nr; i++) {
        grid[i] = (int*)malloc(nc * sizeof(int));
        dp[i] = (long long*)malloc(nc * sizeof(long long));
        for (int j = 0; j < nc; j++) {
            dp[i][j] = -1;
        }
    }


    fp = fopen("input.txt", "r");
    row = 0;
    while ((read = getline(&line, &len, fp)) != -1) {
        for (int j = 0; j < nc; j++) {
            grid[row][j] = line[j] - '0';
        }
        row++;
    }

    fclose(fp);
    if (line) free(line);

    long long total = 0;
    for (int r = 0; r < nr; r++) {
        for (int c = 0; c < nc; c++) {
            if (grid[r][c] == 0) {
                total += dfs(r, c);
            }
        }
    }

    printf("%lld\n", total);
    for (int i = 0; i < nr; i++) {
        free(grid[i]);
        free(dp[i]);
    }
    free(grid);
    free(dp);
    return 0;
}
