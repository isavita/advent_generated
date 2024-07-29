#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LOCATIONS 100
#define MAX_NAME_LEN 20

typedef struct {
    char name[MAX_NAME_LEN];
    int distances[MAX_LOCATIONS];
    int count;
} Location;

Location locations[MAX_LOCATIONS];
int locationCount = 0;

int findLocationIndex(const char *name) {
    for (int i = 0; i < locationCount; i++) {
        if (strcmp(locations[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

void addDistance(const char *from, const char *to, int distance) {
    int fromIndex = findLocationIndex(from);
    int toIndex = findLocationIndex(to);
    
    if (fromIndex == -1) {
        strcpy(locations[locationCount].name, from);
        fromIndex = locationCount++;
    }
    if (toIndex == -1) {
        strcpy(locations[locationCount].name, to);
        toIndex = locationCount++;
    }
    
    locations[fromIndex].distances[toIndex] = distance;
    locations[toIndex].distances[fromIndex] = distance;
}

int calculateRouteDistance(int *route, int size) {
    int sum = 0;
    for (int i = 0; i < size - 1; i++) {
        sum += locations[route[i]].distances[route[i + 1]];
    }
    return sum;
}

int longestRoute(int *route, int depth) {
    if (depth == locationCount) {
        return calculateRouteDistance(route, depth);
    }
    int maxDistance = 0;
    for (int i = depth; i < locationCount; i++) {
        int temp = route[depth];
        route[depth] = route[i];
        route[i] = temp;
        int distance = longestRoute(route, depth + 1);
        if (distance > maxDistance) {
            maxDistance = distance;
        }
        route[i] = route[depth];
        route[depth] = temp;
    }
    return maxDistance;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    char from[MAX_NAME_LEN], to[MAX_NAME_LEN];
    int distance;
    
    while (fscanf(file, "%s to %s = %d", from, to, &distance) == 3) {
        addDistance(from, to, distance);
    }
    fclose(file);
    
    int route[MAX_LOCATIONS];
    for (int i = 0; i < locationCount; i++) {
        route[i] = i;
    }
    
    printf("%d\n", longestRoute(route, 0));
    return 0;
}