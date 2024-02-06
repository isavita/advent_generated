
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LOCATIONS 100
#define MAX_NAME_LENGTH 50
#define MAX_LINE_LENGTH 100

int locationCount = 0;
char locations[MAX_LOCATIONS][MAX_NAME_LENGTH];
int distances[MAX_LOCATIONS][MAX_LOCATIONS];

int findLocationIndex(char* name) {
    for (int i = 0; i < locationCount; i++) {
        if (strcmp(locations[i], name) == 0) {
            return i;
        }
    }
    strcpy(locations[locationCount], name);
    return locationCount++;
}

void readAndParseInput(char* filename) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        exit(1);
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        char from[MAX_NAME_LENGTH], to[MAX_NAME_LENGTH];
        int distance;
        if (sscanf(line, "%s to %s = %d", from, to, &distance) == 3) {
            int fromIndex = findLocationIndex(from);
            int toIndex = findLocationIndex(to);
            distances[fromIndex][toIndex] = distance;
            distances[toIndex][fromIndex] = distance; // Assuming symmetric distances
        }
    }

    fclose(file);
}

int calculateRouteDistance(int* route, int n) {
    int sum = 0;
    for (int i = 0; i < n - 1; i++) {
        sum += distances[route[i]][route[i + 1]];
    }
    return sum;
}

void permute(int* arr, int i, int n, int* minDistance) {
    if (i == n) {
        int dist = calculateRouteDistance(arr, n);
        if (*minDistance == -1 || dist < *minDistance) {
            *minDistance = dist;
        }
        return;
    }
    for (int j = i; j < n; j++) {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;

        permute(arr, i + 1, n, minDistance);

        temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
}

int findShortestRoute() {
    int minDistance = -1;
    int route[MAX_LOCATIONS];
    for (int i = 0; i < locationCount; i++) {
        route[i] = i;
    }
    permute(route, 0, locationCount, &minDistance);
    return minDistance;
}

int main() {
    memset(distances, 0, sizeof(distances));
    readAndParseInput("input.txt");

    int minDistance = findShortestRoute();
    printf("%d\n", minDistance);

    return 0;
}
