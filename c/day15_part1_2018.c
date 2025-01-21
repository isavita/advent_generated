
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

#define MAX_MAP_SIZE 100
#define MAX_UNITS 100

typedef enum {
    KindSpace = 1,
    KindElf = 2,
    KindGoblin = 4,
    KindWall = 8,
    KindHighlight = 16
} Kind;

typedef struct {
    int x, y;
} Coordinate;

typedef struct Tile Tile;
typedef struct Unit Unit;

struct Tile {
    Kind kind;
    int x, y;
    Unit *unit;
    Tile* neighbors[4];
};

struct Unit {
    Kind kind;
    int hitpoints;
    int power;
    Tile *tile;
};

typedef struct {
    Tile* tiles[MAX_MAP_SIZE][MAX_MAP_SIZE];
    int rows;
    int cols;
} Map;

typedef struct {
    Unit* units[MAX_UNITS];
    int count;
} UnitList;

typedef struct {
    Map map;
    UnitList units;
} Cave;

const Coordinate offsets[] = {
    {0, -1},
    {-1, 0},
    {1, 0},
    {0, 1}
};

const int defaultHitpoints = 200;
const int defaultPower = 3;

bool isUnit(int bit) {
    return (KindElf|KindGoblin)&bit;
}

void initCave(Cave* cave){
    cave->units.count = 0;
    for (int y = 0; y < MAX_MAP_SIZE; y++) {
        for(int x = 0; x < MAX_MAP_SIZE; x++){
            cave->map.tiles[y][x] = NULL;
        }
    }
    cave->map.rows = 0;
    cave->map.cols = 0;
}

Unit* newUnit(Tile* tile, Kind kind, int elfPower) {
    Unit* unit = (Unit*)malloc(sizeof(Unit));
    if(!unit){
        perror("Failed to allocate memory for unit.");
        exit(EXIT_FAILURE);
    }
    unit->kind = kind;
    unit->hitpoints = defaultHitpoints;
    unit->power = defaultPower;
    unit->tile = tile;
    tile->unit = unit;
    if(unit->kind == KindElf){
        unit->power = elfPower;
    }
    return unit;
}

void parseMap(Cave* cave, char** input, int elfPower) {
    int y = 0;
    while (input[y] != NULL) {
        int rowLength = strlen(input[y]);
        if(rowLength == 0) break;
        if(cave->map.cols == 0){
            cave->map.cols = rowLength;
        }
        for (int x = 0; x < rowLength; x++) {
            char c = input[y][x];
            Kind kind;
            switch(c){
                case '.': kind = KindSpace; break;
                case 'E': kind = KindElf; break;
                case 'G': kind = KindGoblin; break;
                case '#': kind = KindWall; break;
                default: kind = KindWall; break;
            }

            Tile* tile = (Tile*)malloc(sizeof(Tile));
            if(!tile){
                perror("Failed to allocate memory for tile.");
                exit(EXIT_FAILURE);
            }
            tile->kind = kind;
            tile->x = x;
            tile->y = y;
            tile->unit = NULL;
            cave->map.tiles[y][x] = tile;

             if(isUnit(kind)){
                 Unit* unit = newUnit(tile, kind, elfPower);
                 cave->units.units[cave->units.count++] = unit;
            }
        }
        y++;
    }
    cave->map.rows = y;

    for (int y = 0; y < cave->map.rows; y++) {
        for (int x = 0; x < cave->map.cols; x++) {
            Tile* t = cave->map.tiles[y][x];
            if(t){
                for(int i = 0; i < 4; i++){
                    int nx = x + offsets[i].x;
                    int ny = y + offsets[i].y;
                    if (nx >= 0 && nx < cave->map.cols && ny >= 0 && ny < cave->map.rows && cave->map.tiles[ny][nx]){
                        t->neighbors[i] = cave->map.tiles[ny][nx];
                    } else {
                         t->neighbors[i] = NULL;
                    }
                }
            }
        }
    }

}

void freeCave(Cave* cave){
     for (int y = 0; y < cave->map.rows; y++) {
        for (int x = 0; x < cave->map.cols; x++) {
           if(cave->map.tiles[y][x])
               free(cave->map.tiles[y][x]);
        }
    }
      for (int i = 0; i < cave->units.count; i++) {
         free(cave->units.units[i]);
    }
}


void printMap(Cave* cave, Tile* highlight) {
    for (int y = 0; y < cave->map.rows; y++) {
        for (int x = 0; x < cave->map.cols; x++) {
            Tile* t = cave->map.tiles[y][x];
            if (t == highlight) {
                printf("@");
            } else {
                switch (t->kind) {
                    case KindSpace: printf("."); break;
                    case KindElf: printf("E"); break;
                    case KindGoblin: printf("G"); break;
                    case KindWall: printf("#"); break;
                }
            }
        }
        printf("\n");
    }
}

int compareUnits(const void* a, const void* b) {
    Unit* unitA = *(Unit**)a;
    Unit* unitB = *(Unit**)b;
    if (unitA->tile->y == unitB->tile->y) {
        return unitA->tile->x - unitB->tile->x;
    }
    return unitA->tile->y - unitB->tile->y;
}

void sortUnits(UnitList* units){
    qsort(units->units, units->count, sizeof(Unit*), compareUnits);
}


int status(Cave* cave, bool* stillFighting) {
    bool elves = false;
    bool goblins = false;
    int hp = 0;

    for(int i = 0; i < cave->units.count; i++){
        Unit* u = cave->units.units[i];
        if(u->hitpoints <= 0) continue;
        if(u->kind == KindElf) elves = true;
        else goblins = true;
        hp += u->hitpoints;
    }

    *stillFighting = elves && goblins;
    return hp;
}


void removeTheDead(Cave* cave) {
    int newCount = 0;
    for(int i = 0; i < cave->units.count; i++){
       if(cave->units.units[i]->hitpoints > 0){
          cave->units.units[newCount++] = cave->units.units[i];
       }
    }
    cave->units.count = newCount;
}


void removeUnit(Cave* cave, Unit* unit) {
    unit->tile->kind = KindSpace;
    unit->tile->unit = NULL;
    unit->tile = NULL;
}


typedef struct {
   Tile* tile;
    int distance;
} QueueItem;

#define QUEUE_SIZE  (MAX_MAP_SIZE * MAX_MAP_SIZE)

typedef struct {
    QueueItem items[QUEUE_SIZE];
    int head;
    int tail;
} Queue;

void initQueue(Queue* q) {
    q->head = 0;
    q->tail = 0;
}

bool isQueueEmpty(const Queue* q) {
    return q->head == q->tail;
}

void enqueue(Queue* q, Tile* tile, int distance) {
    if ((q->tail + 1) % QUEUE_SIZE == q->head) {
      fprintf(stderr, "Queue is full.\n");
      exit(EXIT_FAILURE);
    }
    q->items[q->tail].tile = tile;
    q->items[q->tail].distance = distance;
    q->tail = (q->tail + 1) % QUEUE_SIZE;
}

QueueItem dequeue(Queue* q) {
    if (isQueueEmpty(q)) {
       fprintf(stderr, "Queue is empty.\n");
       exit(EXIT_FAILURE);
    }
    QueueItem item = q->items[q->head];
    q->head = (q->head + 1) % QUEUE_SIZE;
    return item;
}


typedef struct {
  int distances[MAX_MAP_SIZE][MAX_MAP_SIZE];
  Tile* cameFrom[MAX_MAP_SIZE][MAX_MAP_SIZE];
  int valid;
} DistanceMap;


void initDistanceMap(DistanceMap *dm, Cave *cave) {
    dm->valid = 0;
   for(int y = 0; y < cave->map.rows; y++){
      for(int x = 0; x < cave->map.cols; x++){
        dm->distances[y][x] = -1;
        dm->cameFrom[y][x] = NULL;
      }
   }
}

void findWalkableTiles(Cave* cave, Tile* start, DistanceMap *dm) {
    initDistanceMap(dm, cave);
    Queue q;
    initQueue(&q);
    enqueue(&q, start, 0);
    dm->distances[start->y][start->x] = 0;
    dm->valid = 1;

    while (!isQueueEmpty(&q)) {
        QueueItem currentItem = dequeue(&q);
        Tile* current = currentItem.tile;
        int distance = currentItem.distance;
          for (int i = 0; i < 4; i++) {
            Tile* next = current->neighbors[i];
              if (next && next->kind == KindSpace && dm->distances[next->y][next->x] == -1) {
                  enqueue(&q, next, distance + 1);
                  dm->distances[next->y][next->x] = distance + 1;
                  dm->cameFrom[next->y][next->x] = current;
              }
          }
      }
}

bool targets(Unit* unit, Cave* cave) {
    for(int i = 0; i < cave->units.count; i++){
        Unit* other = cave->units.units[i];
        if(other->kind != unit->kind && other->hitpoints > 0){
            return true;
        }
    }
    return false;
}

Tile* nextTile(Unit* unit, Cave* cave, DistanceMap *dm, Tile** targetTile) {
    int closestTargetDistance = INT_MAX;
    Tile* target = NULL;
    Tile* next = NULL;
    
    
     for(int i = 0; i < cave->units.count; i++){
        Unit* enemy = cave->units.units[i];
        if(enemy->kind == unit->kind || enemy->hitpoints <= 0) continue;
            for(int j = 0; j < 4; j++){
                Tile* neighbor = enemy->tile->neighbors[j];
                if(neighbor && dm->distances[neighbor->y][neighbor->x] != -1 && dm->distances[neighbor->y][neighbor->x] <= closestTargetDistance){
                   if(dm->distances[neighbor->y][neighbor->x] < closestTargetDistance){
                       closestTargetDistance = dm->distances[neighbor->y][neighbor->x];
                       target = neighbor;
                   } else if (neighbor->y < target->y || (neighbor->y == target->y && neighbor->x < target->x)) {
                       target = neighbor;
                   }
                }
            }
    }
  
    if(target){
         *targetTile = target;
         Tile* current = target;
          while(dm->cameFrom[current->y][current->x] != unit->tile){
             current = dm->cameFrom[current->y][current->x];
              if(current == NULL) {
                  return NULL;
              }
          }
        next = current;
    }
     return next;
}


Unit* enemyNeighbor(Unit* unit, Cave* cave) {
    Unit* target = NULL;
    for (int i = 0; i < 4; i++) {
        Tile* neighbor = unit->tile->neighbors[i];
        if (neighbor && neighbor->unit && neighbor->unit->kind != unit->kind && neighbor->unit->hitpoints > 0) {
            if (!target || neighbor->unit->hitpoints < target->hitpoints) {
                target = neighbor->unit;
            }
        }
    }
    return target;
}

void move(Unit* unit, Cave* cave, DistanceMap *dm) {
    if (enemyNeighbor(unit, cave)) {
        return;
    }
    Tile* target = NULL;
    Tile* next = nextTile(unit, cave, dm, &target);
     if (next) {
        next->unit = unit;
        next->kind = unit->kind;
        unit->tile->kind = KindSpace;
        unit->tile->unit = NULL;
        unit->tile = next;
    }
}

bool attack(Unit* unit, Cave* cave) {
    Unit* enemy = enemyNeighbor(unit, cave);
    if (enemy) {
        bool killed = false;
        enemy->hitpoints -= unit->power;
        if (enemy->hitpoints <= 0) {
             removeUnit(cave, enemy);
            killed = true;
        }
        return killed && enemy->kind == KindElf;
    }
    return false;
}

bool tick(Cave* cave, bool stopOnElfDeath, DistanceMap *dm) {
    removeTheDead(cave);
    sortUnits(&cave->units);

    for (int i = 0; i < cave->units.count; i++) {
        Unit* unit = cave->units.units[i];
         if(unit->hitpoints <= 0) continue;
        if (!targets(unit, cave)) {
            return false;
        }
        findWalkableTiles(cave, unit->tile, dm);
        move(unit, cave, dm);
        if (attack(unit, cave) && stopOnElfDeath) {
            return false;
        }
    }
    return true;
}


int combat(char** input) {
    Cave cave;
    initCave(&cave);
    parseMap(&cave, input, defaultPower);
    DistanceMap dm;
  
    int i;
    for ( i = 1; ; i++) {
         bool combat_ongoing;
        int hp = status(&cave, &combat_ongoing);
       
        if (!combat_ongoing) {
             int result = (i - 1) * hp;
            freeCave(&cave);
            return result;
        }
        
        if (!tick(&cave, false, &dm)) {
            i--;
        }
    }
    return -1;
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char* input[MAX_MAP_SIZE];
    char line[1024];
    int lineCount = 0;

    while(fgets(line, sizeof(line), fp)){
        input[lineCount] = strdup(line);
        if(!input[lineCount]){
          perror("Failed to allocate input line.");
           for(int j = 0; j < lineCount; j++){
               free(input[j]);
           }
            fclose(fp);
            exit(EXIT_FAILURE);
        }
       
       size_t len = strlen(input[lineCount]);
        if (len > 0 && input[lineCount][len-1] == '\n') {
            input[lineCount][len-1] = '\0';
         }
         if(len == 1 && input[lineCount][0] == '\0') {
            free(input[lineCount]);
            input[lineCount] = NULL;
            break;
        }
       
        lineCount++;
        if(lineCount >= MAX_MAP_SIZE){
            fprintf(stderr, "Too many lines in the input file.");
             for(int j = 0; j < lineCount; j++){
               free(input[j]);
            }
            fclose(fp);
            exit(EXIT_FAILURE);
        }
    }
    input[lineCount] = NULL;
    fclose(fp);

    int result = combat(input);
    printf("%d\n", result);
     for(int j = 0; input[j] != NULL; j++){
        free(input[j]);
    }
    return 0;
}
