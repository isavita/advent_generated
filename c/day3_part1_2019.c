
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_POINTS 20000
#define HASH_SIZE 40000

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point p;
    int visited;
    struct Node *next;
} Node;

Node* hashTable[HASH_SIZE];

int hash(int x, int y) {
    int h = (x * 31 + y) % HASH_SIZE;
    return h < 0 ? h + HASH_SIZE : h;
}

void insert(int x, int y, int wire) {
    int h = hash(x, y);
    Node* newNode = (Node*) malloc(sizeof(Node));
    newNode->p.x = x;
    newNode->p.y = y;
    newNode->visited = wire;
    newNode->next = hashTable[h];
    hashTable[h] = newNode;
}

Node* find(int x, int y) {
  int h = hash(x,y);
  Node* curr = hashTable[h];
  while(curr != NULL){
      if(curr->p.x == x && curr->p.y == y){
          return curr;
      }
      curr = curr->next;
  }
  return NULL;
}


int abs(int x) {
    return x < 0 ? -x : x;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *line1 = NULL, *line2 = NULL;
    size_t len1 = 0, len2 = 0;
    getline(&line1, &len1, fp);
    getline(&line2, &len2, fp);
    fclose(fp);
    
    
    line1[strcspn(line1, "\r\n")] = 0;
    line2[strcspn(line2, "\r\n")] = 0;

    char *token;
    int x = 0, y = 0, wire=1;
    token = strtok(line1, ",");
    while (token != NULL) {
        char dir = token[0];
        int steps = atoi(token + 1);
        for (int i = 0; i < steps; i++) {
            if (dir == 'U') y++;
            else if (dir == 'D') y--;
            else if (dir == 'L') x--;
            else if (dir == 'R') x++;
            insert(x, y, wire);
        }
         token = strtok(NULL, ",");
    }

    x=0,y=0,wire=2;
     token = strtok(line2, ",");
    int minDistance = INT_MAX;
    while (token != NULL) {
        char dir = token[0];
        int steps = atoi(token + 1);
         for (int i = 0; i < steps; i++) {
            if (dir == 'U') y++;
            else if (dir == 'D') y--;
            else if (dir == 'L') x--;
            else if (dir == 'R') x++;
             Node* found = find(x,y);
             if(found != NULL && found->visited == 1){
               int dist = abs(x) + abs(y);
                 if(dist < minDistance){
                    minDistance = dist;
                 }
             }
             insert(x,y,wire);
        }
        token = strtok(NULL, ",");
    }
    
    printf("%d\n", minDistance);
    free(line1);
    free(line2);
     for (int i = 0; i < HASH_SIZE; i++) {
        Node *curr = hashTable[i];
        while (curr != NULL) {
            Node *temp = curr;
            curr = curr->next;
            free(temp);
        }
    }

    return 0;
}
