
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point from;
    Point to;
} Door;

typedef struct {
    Door* doors;
    int count;
    int capacity;
} DoorMap;

typedef struct {
    Point* points;
    int* distances;
    int count;
    int capacity;
} Queue;

DoorMap* createDoorMap() {
    DoorMap* dm = (DoorMap*)malloc(sizeof(DoorMap));
    dm->doors = NULL;
    dm->count = 0;
    dm->capacity = 0;
    return dm;
}

void freeDoorMap(DoorMap* dm) {
    free(dm->doors);
    free(dm);
}


void addDoor(DoorMap* dm, Point from, Point to) {
    for(int i=0; i<dm->count; i++){
        if((dm->doors[i].from.x == from.x && dm->doors[i].from.y == from.y && dm->doors[i].to.x == to.x && dm->doors[i].to.y == to.y)
           || (dm->doors[i].from.x == to.x && dm->doors[i].from.y == to.y && dm->doors[i].to.x == from.x && dm->doors[i].to.y == from.y) )
           return;
    }

    if (dm->count == dm->capacity) {
        dm->capacity = dm->capacity == 0 ? 1 : dm->capacity * 2;
        dm->doors = (Door*)realloc(dm->doors, dm->capacity * sizeof(Door));
    }
    dm->doors[dm->count].from = from;
    dm->doors[dm->count].to = to;
    dm->count++;
}

bool hasDoor(DoorMap* dm, Point from, Point to){
    for(int i=0; i<dm->count; i++){
        if((dm->doors[i].from.x == from.x && dm->doors[i].from.y == from.y && dm->doors[i].to.x == to.x && dm->doors[i].to.y == to.y)
           || (dm->doors[i].from.x == to.x && dm->doors[i].from.y == to.y && dm->doors[i].to.x == from.x && dm->doors[i].to.y == from.y) )
           return true;
    }
    return false;
}


Point move(Point p, char dir) {
    switch (dir) {
        case 'N': p.y--; break;
        case 'S': p.y++; break;
        case 'E': p.x++; break;
        case 'W': p.x--; break;
    }
    return p;
}

DoorMap* buildMap(const char* regex) {
    DoorMap* dm = createDoorMap();
    Point stack[1000];
    int stack_top = 0;
    Point cp = {0, 0};
    for (int i = 0; regex[i] != '\0'; i++) {
        char c = regex[i];
        if (c == '(') {
            stack[stack_top++] = cp;
        } else if (c == '|') {
            cp = stack[stack_top - 1];
        } else if (c == ')') {
            cp = stack[--stack_top];
        } else {
            Point np = move(cp, c);
            addDoor(dm, cp, np);
            cp = np;
        }
    }
    return dm;
}

Queue* createQueue() {
    Queue* q = (Queue*)malloc(sizeof(Queue));
    q->points = NULL;
    q->distances = NULL;
    q->count = 0;
    q->capacity = 0;
    return q;
}

void freeQueue(Queue* q){
    free(q->points);
    free(q->distances);
    free(q);
}


void enqueue(Queue* q, Point p, int dist) {
    if (q->count == q->capacity) {
        q->capacity = q->capacity == 0 ? 1 : q->capacity * 2;
        q->points = (Point*)realloc(q->points, q->capacity * sizeof(Point));
        q->distances = (int*)realloc(q->distances, q->capacity * sizeof(int));
    }
    q->points[q->count] = p;
    q->distances[q->count] = dist;
    q->count++;
}

bool isEmpty(Queue* q) {
    return q->count == 0;
}

Point dequeue(Queue* q, int* dist) {
    Point p = q->points[0];
    *dist = q->distances[0];
    q->count--;
    for (int i = 0; i < q->count; i++) {
        q->points[i] = q->points[i + 1];
        q->distances[i] = q->distances[i + 1];
    }
    return p;
}


int findFurthestRoom(DoorMap* dm) {
    int maxDoors = 0;
    Queue* q = createQueue();
    enqueue(q, (Point){0, 0}, 0);

    
    int visited_capacity = 1000;
    Point *visited = (Point*) malloc(visited_capacity * sizeof(Point));
    int *distances = (int*) malloc(visited_capacity * sizeof(int));
    int visited_count = 0;
    
    
    while (!isEmpty(q)) {
        int dist;
        Point p = dequeue(q, &dist);
        
        bool already_visited = false;
        for(int i=0; i<visited_count; i++){
            if(visited[i].x == p.x && visited[i].y == p.y){
                already_visited = true;
                break;
            }
        }

        if(already_visited) continue;
        
       if(visited_count == visited_capacity){
            visited_capacity = visited_capacity * 2;
            visited = (Point*) realloc(visited, visited_capacity * sizeof(Point));
            distances = (int*) realloc(distances, visited_capacity * sizeof(int));
        }
       
        visited[visited_count] = p;
        distances[visited_count] = dist;
        visited_count++;
        

        for(int i=0; i<dm->count; i++){
             if (dm->doors[i].from.x == p.x && dm->doors[i].from.y == p.y)
             {
                bool found = false;
                  for(int v=0; v<visited_count; v++){
                        if(visited[v].x == dm->doors[i].to.x && visited[v].y == dm->doors[i].to.y)
                           {
                            found=true;
                            break;
                           }
                  }
                  if(!found){
                      enqueue(q, dm->doors[i].to, dist + 1);
                      if (dist + 1 > maxDoors) {
                            maxDoors = dist + 1;
                        }
                }
             }
              else if (dm->doors[i].to.x == p.x && dm->doors[i].to.y == p.y)
              {
                bool found = false;
                  for(int v=0; v<visited_count; v++){
                        if(visited[v].x == dm->doors[i].from.x && visited[v].y == dm->doors[i].from.y)
                        {
                            found=true;
                            break;
                        }
                    }
                    if(!found){
                    enqueue(q, dm->doors[i].from, dist + 1);
                    if (dist + 1 > maxDoors) {
                            maxDoors = dist + 1;
                        }
                    }
            }
        }
    }
    free(visited);
    free(distances);
    freeQueue(q);
    return maxDoors;
}

int main() {
    FILE* fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* regex = (char*)malloc((file_size + 1) * sizeof(char));
    if (regex == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }
    fread(regex, 1, file_size, fp);
    fclose(fp);
    regex[file_size] = '\0';

    
    DoorMap* dm = buildMap(regex + 1);
    int maxDoors = findFurthestRoom(dm);
    printf("%d\n", maxDoors);
    
    freeDoorMap(dm);
    free(regex);
    return 0;
}
