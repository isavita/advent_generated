
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_POINTS 100000
#define MAX_GRID_SIZE 1000
typedef struct {
    int x, y;
} Point;

typedef struct {
  Point data[MAX_POINTS];
  int size;
} PointArray;

PointArray points;
bool grid[MAX_GRID_SIZE][MAX_GRID_SIZE] = {false};
int floor_level = 0;

int min(int a, int b) {
    return a < b ? a : b;
}

int max(int a, int b) {
    return a > b ? a : b;
}
void add_point(Point p){
    points.data[points.size++] = p;
}

void init_points(){
    points.size = 0;
}
void populate_grid(int x1, int y1, int x2, int y2) {
    if (x1 == x2) {
        for (int y = min(y1, y2); y <= max(y1, y2); y++) {
            grid[x1][y] = true;
            if(y> floor_level) floor_level = y;
        }
    } else {
        for (int x = min(x1, x2); x <= max(x1, x2); x++) {
            grid[x][y1] = true;
             if(y1> floor_level) floor_level = y1;
        }
    }
}
int fill() {
    floor_level++;
    int sands = 0, first_floor_touch = 0;
    while (!grid[500][0]) {
        Point sand = {500, 0};
        bool settled = false;
        while (!settled) {
            Point next_sand[] = {{sand.x,sand.y + 1},{sand.x-1,sand.y+1},{sand.x+1,sand.y+1}};
            bool moved = false;
             for(int i = 0 ; i < 3 ; i++){
                if (!grid[next_sand[i].x][next_sand[i].y])
                {
                    sand = next_sand[i];
                     moved = true;
                     break;
                }
             }
             if(!moved){
                  grid[sand.x][sand.y] = true;
                   settled = true;
             }
            if (sand.y == floor_level) {
                if (first_floor_touch == 0) {
                    first_floor_touch = sands;
                }
                grid[sand.x][sand.y] = true;
                 settled = true;
            }

        }
        sands++;
    }
    return first_floor_touch;
}
int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
   init_points();
    while ((read = getline(&line, &len, fp)) != -1) {
       char *token;
       Point current;
        token = strtok(line," -> ");
        sscanf(token,"%d,%d",&current.x,&current.y);
        add_point(current);
        while(token != NULL)
        {
            token = strtok(NULL," -> ");
             if(token != NULL){
               Point next;
               sscanf(token,"%d,%d",&next.x,&next.y);
                populate_grid(current.x,current.y,next.x,next.y);
                current = next;
                add_point(current);
            }
        }

    }

    printf("%d\n", fill());
    fclose(fp);
    if (line) free(line);
    return 0;
}
