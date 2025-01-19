
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    int q, r;
} Coordinate;

Coordinate directions[] = {
    {1, 0}, {0, 1}, {-1, 1}, {-1, 0}, {0, -1}, {1, -1}
};

int getNeighbors(Coordinate tile, Coordinate *neighbors) {
    for (int i = 0; i < 6; i++) {
        neighbors[i].q = tile.q + directions[i].q;
        neighbors[i].r = tile.r + directions[i].r;
    }
    return 6;
}

typedef struct {
    Coordinate key;
    bool value;
    struct Node *next;
} Node;

typedef struct {
    Node **buckets;
    int capacity;
    int size;
} HashTable;

unsigned int hash(Coordinate key, int capacity) {
    return (abs(key.q * 31 + key.r) % capacity);
}

HashTable* createHashTable(int capacity) {
    HashTable* ht = (HashTable*)malloc(sizeof(HashTable));
    ht->capacity = capacity;
    ht->size = 0;
    ht->buckets = (Node**)calloc(capacity, sizeof(Node*));
    return ht;
}

void freeHashTable(HashTable* ht) {
    if (!ht) return;
    for(int i=0; i<ht->capacity; ++i){
        Node* current = ht->buckets[i];
        while(current){
            Node* temp = current;
            current = current->next;
            free(temp);
        }
    }
    free(ht->buckets);
    free(ht);
}


bool get(HashTable *ht, Coordinate key) {
    unsigned int index = hash(key, ht->capacity);
    Node *current = ht->buckets[index];
    while (current) {
        if (current->key.q == key.q && current->key.r == key.r) {
            return current->value;
        }
        current = current->next;
    }
    return false;
}


void set(HashTable* ht, Coordinate key, bool value) {
    unsigned int index = hash(key, ht->capacity);
    Node* current = ht->buckets[index];
    
    while (current) {
        if (current->key.q == key.q && current->key.r == key.r) {
            current->value = value;
            return;
        }
        current = current->next;
    }
   
    Node* newNode = (Node*)malloc(sizeof(Node));
    newNode->key = key;
    newNode->value = value;
    newNode->next = ht->buckets[index];
    ht->buckets[index] = newNode;
    ht->size++;
    if (ht->size > ht->capacity * 0.75) {
        int newCapacity = ht->capacity * 2;
        Node **newBuckets = (Node **)calloc(newCapacity, sizeof(Node*));
        for (int i = 0; i < ht->capacity; ++i) {
            Node *current = ht->buckets[i];
            while (current) {
                Node *temp = current;
                current = current->next;
                unsigned int newIndex = hash(temp->key, newCapacity);
                temp->next = newBuckets[newIndex];
                newBuckets[newIndex] = temp;
            }
        }
        free(ht->buckets);
        ht->buckets = newBuckets;
        ht->capacity = newCapacity;
    }
}

int countBlackTiles(HashTable* ht) {
    int count = 0;
    for (int i = 0; i < ht->capacity; i++) {
        Node* current = ht->buckets[i];
        while(current){
            if(current->value)
                count++;
            current = current->next;
        }
    }
    return count;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    HashTable *blackTiles = createHashTable(1024);
    char line[256];

    while (fgets(line, sizeof(line), file)) {
        Coordinate coord = {0, 0};
        for (int i = 0; line[i] != '\0' && line[i] != '\n';) {
             char dir[3] = {0};
            if (line[i] == 'e' || line[i] == 'w') {
                 dir[0] = line[i];
                 i++;
            } else if (line[i] == 'n' || line[i] == 's') {
                 dir[0] = line[i];
                 dir[1] = line[i+1];
                i += 2;
            }
            
            
            int dirIndex;
            if (strcmp(dir, "e") == 0) dirIndex=0;
            else if (strcmp(dir, "se")==0) dirIndex=1;
            else if (strcmp(dir, "sw")==0) dirIndex=2;
            else if (strcmp(dir, "w")==0) dirIndex=3;
            else if (strcmp(dir, "nw")==0) dirIndex=4;
            else if (strcmp(dir, "ne")==0) dirIndex=5;
            coord.q += directions[dirIndex].q;
            coord.r += directions[dirIndex].r;
        }
         bool currentValue = get(blackTiles, coord);
         set(blackTiles, coord, !currentValue);
    }
    fclose(file);

    for (int day = 0; day < 100; day++) {
        HashTable *tilesToCheck = createHashTable(blackTiles->capacity);
        for (int i = 0; i < blackTiles->capacity; ++i) {
           Node* current = blackTiles->buckets[i];
            while(current) {
                if(current->value) {
                    set(tilesToCheck, current->key, true);
                    Coordinate neighbors[6];
                    getNeighbors(current->key, neighbors);
                    for(int j=0; j<6; j++)
                        set(tilesToCheck, neighbors[j], true);
                }
                current = current->next;
            }
        }
        
        HashTable *newBlackTiles = createHashTable(tilesToCheck->capacity);
        for (int i = 0; i < tilesToCheck->capacity; i++) {
           Node* current = tilesToCheck->buckets[i];
            while(current) {
                int blackNeighborCount = 0;
                Coordinate neighbors[6];
                getNeighbors(current->key, neighbors);
                for (int j = 0; j < 6; j++) {
                   if(get(blackTiles, neighbors[j]))
                       blackNeighborCount++;
                }
                
                if (get(blackTiles, current->key) && (blackNeighborCount == 1 || blackNeighborCount == 2)) {
                    set(newBlackTiles, current->key, true);
                } else if (!get(blackTiles, current->key) && blackNeighborCount == 2) {
                    set(newBlackTiles, current->key, true);
                }
                current=current->next;
            }
        }
        freeHashTable(blackTiles);
        freeHashTable(tilesToCheck);
        blackTiles = newBlackTiles;
    }
    printf("%d\n", countBlackTiles(blackTiles));
    freeHashTable(blackTiles);
    return 0;
}
