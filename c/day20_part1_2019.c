
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SIZE 200

typedef struct {
    int x, y;
} Point;

typedef struct {
    int xMax, yMax;
    char grid[MAX_SIZE][MAX_SIZE];
    Point aa, zz;
    Point teleport[MAX_SIZE * MAX_SIZE];
    char portalName[MAX_SIZE * MAX_SIZE][3];
    bool isOuter[MAX_SIZE * MAX_SIZE];
    int teleportCount;
    int portalCount;
} Map;

Point getPoint(int x, int y) {
    Point p;
    p.x = x;
    p.y = y;
    return p;
}

bool isLetter(char c) {
    return c >= 'A' && c <= 'Z';
}


Point getNeighbours(Point p, int dir) {
    switch(dir) {
        case 0: return getPoint(p.x, p.y + 1);
        case 1: return getPoint(p.x + 1, p.y);
        case 2: return getPoint(p.x, p.y - 1);
        case 3: return getPoint(p.x - 1, p.y);
    }
    return p;
}



bool extractPortal(Map *m, Point p, char *portalName, Point *portalPoint) {
    char c1 = m->grid[p.x][p.y];
    
     if (isLetter(m->grid[p.x + 1][p.y])) {
        char c2 = m->grid[p.x+1][p.y];
        portalName[0] = c1;
        portalName[1] = c2;
        portalName[2] = '\0';
        Point temp = getPoint(p.x + 2, p.y);
        if (m->grid[temp.x][temp.y] == '.') {
            *portalPoint = temp;
            return true;
        }

        temp = getPoint(p.x - 1, p.y);
        if (m->grid[temp.x][temp.y] == '.') {
            *portalPoint = temp;
            return true;
        }
    }


     if (isLetter(m->grid[p.x][p.y+1])) {
        char c2 = m->grid[p.x][p.y+1];
        portalName[0] = c1;
        portalName[1] = c2;
        portalName[2] = '\0';

        Point temp = getPoint(p.x, p.y + 2);
        if (m->grid[temp.x][temp.y] == '.') {
            *portalPoint = temp;
            return true;
        }

         temp = getPoint(p.x, p.y - 1);
        if (m->grid[temp.x][temp.y] == '.') {
           *portalPoint = temp;
            return true;
        }
    }
    return false;
}


Map* parse() {
    Map *m = (Map*)malloc(sizeof(Map));
     if (m == NULL) {
        perror("Failed to allocate memory for map");
        exit(EXIT_FAILURE);
    }
    memset(m, 0, sizeof(Map));

    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    char line[MAX_SIZE];
    int i = 0;
    m->xMax = 0;
    m->yMax = 0;


    while (fgets(line, sizeof(line), file) != NULL) {
         int len = strlen(line);
         if (line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }

         if (len > m->yMax) {
            m->yMax = len;
         }

         for (int j = 0; j < len; j++) {
            m->grid[i][j] = line[j];
        }

        i++;
    }
    m->xMax = i;


    char cache[MAX_SIZE * MAX_SIZE][3];
    Point cachePoint[MAX_SIZE * MAX_SIZE];
    int cacheCount = 0;

    for (int x = 0; x < m->xMax; x++) {
        for (int y = 0; y < m->yMax; y++) {
             Point curr = getPoint(x,y);
             if (!isLetter(m->grid[curr.x][curr.y])) {
                continue;
             }

            char pName[3];
            Point pPoint;
           
           if (!extractPortal(m, curr, pName, &pPoint)) {
               continue;
           }


            int idx = m->portalCount;
            strcpy(m->portalName[idx],pName);
            
            if(strcmp(pName,"AA") == 0) {
                m->aa = pPoint;
                m->isOuter[idx] = true;
                m->portalCount++;
                continue;
            }
              if(strcmp(pName,"ZZ") == 0) {
                m->zz = pPoint;
                m->isOuter[idx] = true;
                m->portalCount++;
                continue;
            }
            
            bool found = false;

           for (int k = 0; k < cacheCount; k++) {
                if (strcmp(cache[k], pName) == 0) {
                    m->teleport[pPoint.x * MAX_SIZE + pPoint.y] = cachePoint[k];
                    m->teleport[cachePoint[k].x*MAX_SIZE+cachePoint[k].y] = pPoint;
                    found = true;
                    break;
                }
            }


             if (!found) {
                strcpy(cache[cacheCount], pName);
                cachePoint[cacheCount] = pPoint;
                 cacheCount++;
             }

             int cx = pPoint.x;
             int cy = pPoint.y;
             
            if (cx == 0 || cy == 0 || cx == m->xMax - 2 || cy == m->yMax - 2 ) {
                    m->isOuter[idx] = true;
                } else {
                  m->isOuter[idx] = false;
                }

            m->portalCount++;

        }
    }


    fclose(file);
    return m;
}



int BFS(Map *m) {
    bool discovered[MAX_SIZE][MAX_SIZE] = {false};
     Point queue[MAX_SIZE * MAX_SIZE];
    int head = 0, tail = 0;

    discovered[m->aa.x][m->aa.y] = true;
    queue[tail++] = m->aa;
    int depth = 0;

    while (head < tail) {
        int levelSize = tail - head;
           for (int k = 0; k < levelSize; k++) {
             Point curr = queue[head++];

             if (curr.x == m->zz.x && curr.y == m->zz.y) {
                return depth;
             }

            for(int i = 0; i < 4; i++){
                Point next = getNeighbours(curr,i);
                if(next.x < 0 || next.x >= m->xMax || next.y < 0 || next.y >= m->yMax )
                    continue;
                char dest = m->grid[next.x][next.y];

                switch (dest) {
                    case '#':
                        continue;
                    case '.':
                        if (!discovered[next.x][next.y]) {
                            discovered[next.x][next.y] = true;
                             queue[tail++] = next;
                        }
                         break;
                    default:
                     if(isLetter(dest)){

                         Point teleport_point = m->teleport[curr.x*MAX_SIZE + curr.y];
                            if (!discovered[teleport_point.x][teleport_point.y]) {
                                 discovered[teleport_point.x][teleport_point.y] = true;
                                queue[tail++] = teleport_point;
                            }
                    }
                    break;
                }

            }
        }

         depth++;
    }
      
    return -1;
}


int main() {
    Map *m = parse();
     printf("%d\n", BFS(m));
     free(m);
    return 0;
}
