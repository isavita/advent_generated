
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <float.h>

typedef struct {
    int x, y;
    double angle;
    double dist;
} Asteroid;

int readAsteroids(const char *filename, bool ***grid, int *rows, int *cols) {
    FILE *fp = fopen(filename, "r");
    if (!fp) return 1;
    char line[1024];
    *rows = 0;
    *cols = 0;
    *grid = NULL;
    while (fgets(line, sizeof(line), fp)) {
        int len = 0;
        while (line[len] && line[len] != '\n') len++;
        if (*rows == 0) *cols = len;
        bool *row = malloc(len * sizeof(bool));
        if (!row) { fclose(fp); return 1; }
        for (int i = 0; i < len; i++) row[i] = line[i] == '#';
        *grid = realloc(*grid, (*rows + 1) * sizeof(bool *));
        if (!*grid) { free(row); fclose(fp); return 1; }
        (*grid)[*rows] = row;
        (*rows)++;
    }
    fclose(fp);
    return 0;
}

int compareAsteroid(const void *a, const void *b) {
    const Asteroid *aA = a, *bA = b;
    if (aA->angle < bA->angle) return -1;
    if (aA->angle > bA->angle) return 1;
    if (aA->dist < bA->dist) return -1;
    if (aA->dist > bA->dist) return 1;
    return 0;
}

Asteroid* vaporize(bool **grid, int rows, int cols, int sx, int sy, int *count) {
    int cap = 0, size = 0;
    Asteroid *list = NULL;
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            if (grid[y][x] && !(x == sx && y == sy)) {
                double ang = atan2((double)(y-sy), (double)(x-sx));
                double d = hypot((double)(x-sx), (double)(y-sy));
                if (ang < -M_PI/2) ang += 2*M_PI;
                if (size >= cap) {
                    cap = cap ? cap*2 : 8;
                    list = realloc(list, cap * sizeof(Asteroid));
                }
                list[size].x = x;
                list[size].y = y;
                list[size].angle = ang;
                list[size].dist = d;
                size++;
            }
        }
    }
    qsort(list, size, sizeof(Asteroid), compareAsteroid);
    int resCount = 0;
    double last = -DBL_MAX;
    Asteroid *vapor = malloc(size * sizeof(Asteroid));
    for (int i = 0; i < size; i++) {
        if (fabs(list[i].angle - last) > 1e-9) {
            vapor[resCount++] = list[i];
            last = list[i].angle;
        }
    }
    free(list);
    *count = resCount;
    return vapor;
}

void bestLocation(bool **grid, int rows, int cols, int *bx, int *by, int *max) {
    *max = 0;
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) if (grid[y][x]) {
            double *ang = malloc(rows*cols*sizeof(double));
            int ac = 0, seen = 0;
            for (int y2 = 0; y2 < rows; y2++)
                for (int x2 = 0; x2 < cols; x2++)
                    if (grid[y2][x2] && !(x2==x && y2==y)) {
                        double a = atan2((double)(y2-y),(double)(x2-x));
                        bool found = false;
                        for (int k=0;k<ac;k++) if (fabs(ang[k]-a)<1e-9){found=true;break;}
                        if (!found){ang[ac++]=a;seen++;}
                    }
            if (seen > *max){*max=seen;*bx=x;*by=y;}
            free(ang);
        }
    }
}

int main(int argc, char *argv[]) {
    @autoreleasepool {
        bool **grid; int r,c;
        if (readAsteroids("input.txt",&grid,&r,&c)) return 1;
        int bx,by,mx;
        bestLocation(grid,r,c,&bx,&by,&mx);
        int vapCount;
        Asteroid *vap = vaporize(grid,r,c,bx,by,&vapCount);
        if (!vap){for(int i=0;i<r;i++)free(grid[i]);free(grid);return 1;}
        if (vapCount>=200){
            int val = vap[199].x*100+vap[199].y;
            printf("%d\n",val);
        }
        free(vap);
        for(int i=0;i<r;i++)free(grid[i]);free(grid);
        return 0;
    }
}
