
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 1000

typedef struct {
    int y, x;
} Point;

typedef struct {
    Point *coords;
    int count;
    int capacity;
} PointList;

typedef struct {
    Point key;
    bool value;
} Antinode;

typedef struct {
    Antinode *items;
    int count;
    int capacity;
} AntinodeMap;

int hash(Point p) {
  return p.y * 1001 + p.x;
}

PointList* initPointList(){
    PointList* list = malloc(sizeof(PointList));
    list->coords = NULL;
    list->count = 0;
    list->capacity = 0;
    return list;
}
void addPoint(PointList* list, Point p) {
    if (list->count == list->capacity) {
        list->capacity = list->capacity == 0 ? 4 : list->capacity * 2;
        list->coords = realloc(list->coords, list->capacity * sizeof(Point));
    }
    list->coords[list->count++] = p;
}

void freePointList(PointList* list){
    free(list->coords);
    free(list);
}

AntinodeMap* initAntinodeMap() {
    AntinodeMap* map = malloc(sizeof(AntinodeMap));
    map->items = NULL;
    map->count = 0;
    map->capacity = 0;
    return map;
}
void addAntinode(AntinodeMap* map, Point key) {
    for (int i=0; i < map->count; i++){
      if(map->items[i].key.x == key.x && map->items[i].key.y == key.y) return;
    }
    if (map->count == map->capacity) {
        map->capacity = map->capacity == 0 ? 4 : map->capacity * 2;
        map->items = realloc(map->items, map->capacity * sizeof(Antinode));
    }
    map->items[map->count].key = key;
    map->items[map->count].value = true;
    map->count++;
}

void freeAntinodeMap(AntinodeMap* map){
  free(map->items);
  free(map);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char grid[MAX_GRID_SIZE][MAX_GRID_SIZE + 1];
    int h = 0;
    while (fgets(grid[h], MAX_GRID_SIZE + 1, file) != NULL) {
        grid[h][strcspn(grid[h], "\n")] = 0;
        h++;
    }
    fclose(file);

    int w = strlen(grid[0]);

    PointList* antennas[256] = {NULL};
    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            char c = grid[y][x];
            if (c != '.') {
              if(antennas[c] == NULL){
                antennas[c] = initPointList();
              }
              addPoint(antennas[c], (Point){y,x});
            }
        }
    }

    AntinodeMap* antinodes = initAntinodeMap();

    for (int i = 0; i < 256; i++) {
      PointList* coords = antennas[i];
        if (coords != NULL) {
            int n = coords->count;
            for (int j = 0; j < n; j++) {
                for (int k = j + 1; k < n; k++) {
                    Point A = coords->coords[j];
                    Point B = coords->coords[k];
                    Point P1 = {2 * A.y - B.y, 2 * A.x - B.x};
                    Point P2 = {2 * B.y - A.y, 2 * B.x - A.x};
                    if (P1.y >= 0 && P1.y < h && P1.x >= 0 && P1.x < w) {
                      addAntinode(antinodes, P1);
                    }
                    if (P2.y >= 0 && P2.y < h && P2.x >= 0 && P2.x < w) {
                      addAntinode(antinodes, P2);
                    }
                }
            }
            freePointList(coords);
        }
    }


    printf("%d\n", antinodes->count);
    freeAntinodeMap(antinodes);

    return 0;
}
